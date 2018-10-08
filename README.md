# Class Annotations
This package introduces class annotations. Class annotations are first class objects and that attached to classes. 
Basically a class annation is a subclass of ClassAnnotation and a class to use it should use the <classAnnotation> on a class side method. 

## Class annotation creation

To create a new annotation just define a subclass of the class ClassAnnotation:

```Smalltalk
ClassAnnotation subclass: #MySpecialAnnotation
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'MyPackage'
```

Then add a new class side method to the class which you want to annotate. The class side method should return an instance of the annotation. Pay attention that the implementation may cache the returned value. In addition, the method returning the annotation instance should be marked with pragma #classAnnotation. 

For example, the following method specialAnnotationExample defines that the class MyClass is annotated by the class annotation MySpecialAnnotation. 

```Smalltalk
MyClass class >> specialAnnotationExample
	<classAnnotation>
	
	^ MySpecialAnnotation new
```

Once classes are annotated, they can be queried. 


## Annotation queries
There are two ways how to query annotations:

### 1) You can ask the concrete annotation class for all its registered instances:

The following message will return the instance of the class MySpecialAnnotation attached to the class MyClass above. 
```Smalltalk
MySpecialAnnotation registeredInstances
```

You can also restrict the query of annotations to a specific class. 
```Smalltalk
MySpecialAnnotation registeredInstancesAnnotating: MyClass
```

Finally you can iterate on all the registered annotation instances. 
```Smalltalk
MySpecialAnnotation registeredInstancesDo: [:each | each logCr].
```

### 2) And you can ask a given class for all its attached annotations:

Here we ask the annotated class itself for its annotations. 
```Smalltalk
MyClass classAnnotations
MyClass classAnnotationsDo: [:each | each logCr]
```

Notice that each annotation includes the annotated class and the selector of the method used to declare that a class uses this annotation (called the declaring method). 

```Smalltalk
MyClass classAnnotationsDo: 
	[:anAnnotation | 
		'Class ', anAnnotation annotatedClass logCr.
		'is annotated via' , anAnnotation selector logCr.
		]
```


All annotations are cached in special registry (look at ClassAnnotationRegistry). Therefore it is cheap to query them.

## Features

### Extending classes with meta information
Because annotations are declared in methods, they offer the interesting feature that meta information can extended by external packages. We can use package extension to extend a class with meta information from external packages.

Just define declaration methods as class extensions, and when your package will be loaded the new annotations will be added to the existing class.

### Annotation instantiation
There is no special way how to instantiate annotation instances. It is up to your domain. An annotation is a plain object and can be stateful. You can add any domain specific variables to your annotations and add constructors to initialize them in the declarating methods. We give an example here after:

The internal state of annotation is initialized during registry creation.  Users should not think about it. 

### Annotating Annotations
Annotations are just normal classes without any restrictions. You can also attach annotations to annotations like in other languages.

### Forbidden annotation
Annotations can forbid annotating of particular classes. For example it can forbid abstract classes.
```Smalltalk
MySpecificAnnotation >> isForbidden
      ^annotatedClass isAbstract
```
Such annotations will not be added to the registry and forbidden classes will not include them.

## Contextual annotations
Any annotation can be defined with a context where it can be used. 
Imaging that you want annotate command classes with shortcuts for specific widgets:

```Smalltalk
MyCommand class >> widget1ShortcutAnnotation
   <classAnnotation>
   ^ShortcutAnnotation using: $r meta for: MyWidgetClass1

MyCommand class >> widget2ShortcutAnnotation
   <classAnnotation>
   ^ShortcutAnnotation using: $t meta for: MyWidgetClass2
```
It means that the same command MyCommand is bound the shortcut r in MyWidgetClass1 and the shortcut t in MyWidgetClass2.

Then you can query all shortcuts which should be active for a concrete widget: 
```Smalltalk
ShortcutAnnotation activeAnnotationsInContext: aMyWidgetInstance
ShortcutAnnotation activeAnnotationsInContext: aMyWidgetInstance do: aBlock
```

@@I do not get how this is working@@

Context describes annotation users @@I do not get this sentence@@. And for contextual annotation lookup an instance of user should be provided. In that example it is aMyWidgetInstance.

Declaring a context using classes is simplest way how to restrict users which see annotation. By default a given class is converted to SimpleAnnotationContext instance using #asAnnotationContext message. Then during an annotation query, it simply asks #isKindOf: for given user instance. 

### Redefining active annotation
For advanced scenarios you can implement more complex annotation context and define specific DSL to use them for annotations and queries.
 
Any annotation class can redefine the meaning of active annotation with extra conditions. For example it can delegate decision to annotated class itself:
```Smalltalk
ShortcutAnnotation >> isActiveInContext: aMyWidgetInstance
   ^(super isActiveInContext: aMyWidgetInstance)
        and: [annotatedClass canBeUsedInWidget: aMyWidgetInstance]
```

But for some scenarios you may need to query annotations according to original "active" definition despite of extra conditions. For such cases the "visibility" of annotation is introduced: the annotation is visible if it is declared for given user:
```Smalltalk
ClassAnnotation >> isVisibleInContext: anAnnotationUser
	^activeContext describes: anAnnotationUser
```
So the visible annotation is not necessary active. But active annotation is always visible for given user:
```Smalltalk
ClassAnnotation >> isActiveInContext: anAnnotationUser
	^self isVisibleInContext: anAnnotationUser
```
Imaging that you want annotate commands with context menu information (where they should be executed). In that case disabled menu items can represent commands which are visible for application but not active for it (because selected item is not appropriate).
  
To query visible annotations there are few methods:
```Smalltalk
ContextMenuAnnotation visibleInstancesInContext: anAnnotationUser
ContextMenuAnnotation visibleInstancesInContext: anAnnotationUser do: aBlock
```

## Annotation priority
For particular scenarios, it can be important to define the order in which annotations are processed.
For example in context menu, this order can be used to sort menu items. 
For shortcuts, it allows the override of existing shortcuts of application. 

So the concept of annotation priority solves this point: Any annotation can define it. Annotations with greater priority value is processed first by enumeration methods:
- registeredInstancesDo:
- activeInstancesInContext:do:
- visibleInstancesInContext:do:

Priority is held as instance variable. So you can specify it in declaration method:
```Smalltalk
MyCommand class>>shortcutAnnotation
   <classAnnotation>
   ^(ShortcutAnnotation using: $r meta for: MyWidgetClass) 
       priority: 1000
```
Of course for your annotations, you can add instantiation methods which are suitable for your domain.

In some cases you would need an override order in which annotations should be sorted. For example, you can say that command with greater priority should be at the end of menu.
For such cases you can override class side method #createContainerForRegistry:
```Smalltalk
MySpecificAnnotation class>>createContainerForRegistry
     ^SortedCollection sortBlock: #priority ascending
``` 

## Redefined annotations
All annotations are collected from methods and cached in default ClassAnnotationRegistry instance. 
There is special mechanism to redefine these instances. When the cache is updated all redefined annotations are restored. @@No idea what it means@@ @@I have no idea about what is a redefine annotation. To me it looks like this is more than needed for annotation. @@

To redefine a particular annotation, use #redefineBy: message with a block which will modify properties to original instance.
For example following code allows one to redefine the shortcut of #browse command in Calypso: 
```Smalltalk
(ClySpawnFullBrowserCommand classAnnotationsAt: #browserShortcutActivation)
	redefineBy: [:shortcut | shortcut keyCombination: $o meta ].
```	
Try evaluate it and press cmd+o on selected item in the browser. It will open new browser window.
You can notice that old shortcut cmd+b is not working anymore.

Try manualy reset annotation cache to see that redefined shortcut is still working:
```Smalltalk
ClassAnnotation resetCache.
(ClySpawnFullBrowserCommand classAnnotationsAt: #browserShortcutActivation) inspect
```	
To inspect all redefined annotations ask annotation class: 
```Smalltalk
CmdShortcutCommandActivation redefinedInstances
```	
Redefined instances are stored in class side variable #redefinedInstances. 
It is a dictionary which keys are new redefining annotations and values are original annotations collected from methods.
Notice that key and value are equal objects because annotations define equality using annotated class and declaration selector.
So dictionary items can be accessed using both objects.

To check that an annotation is redefined, use #isRedefined message: 
```Smalltalk
(ClySpawnFullBrowserCommand classAnnotationsAt: #browserShortcutActivation)
	isRedefined
```
You can ask what instance was redefined: 
```Smalltalk
(ClySpawnFullBrowserCommand classAnnotationsAt: #browserShortcutActivation)
	redefinedInstance
```		
	
To revert redefined annotation use #revertRedefinedInstance message: 
```Smalltalk
(ClySpawnFullBrowserCommand classAnnotationsAt: #browserShortcutActivation)
	revertRedefinedInstance
```
This script will revert back old browse shortcut cmd+b (which is defined in annotation declaration method).

You can also revert all redefined annotations:
```Smalltalk
CmdShortcutCommandActivation revertRedefinedInstances
```
Redefining logic is very suitable mechanism to override system behavior which depends on annotations without changing the code.

It can be used to manage particular kind of annotations in settings browser. 
For example shortcut annotations based on Commander are available in Setting browser. Users can explore and edit all shortcuts in the system. All these settings are persistable.

## Annotation registry
The cache of class annotations is managed by default instance of ClassAnnotationRegistry. It is subscribed on system changes and it updates the cache automatically when changes affect class annotations.

There are several scenarios when an update happens:

- User adds, removes or modifies the method which defines class annotation
    - the registry updates all inherited annotations of all subclasses of the class defining affected method
- User creates subclass of annotated class
    - the registry collects all inherited annotations for new class
- User removes a class
- User changes the superclass of annotated class
    - the registry updates all inherited annotations of this class
- User adds, removes or modifies annotation dependency methods

### Annotation dependency methods
Methods defining annotations can call other methods which can affect the annotation state. Therefore the annotation registry should be updated when such dependency methods are modified. For this purpose they should be marked with pragma classAnnotationDependency.

For example CmdShortcutCommandActivation annotation provides reusable methods for rename and remove shorcuts: cmd+r and cmd+x. You can annotate commands using them: 
```Smalltalk
MyRenameCommand class>>shortcutActivation
   <classAnnotation>
   ^CmdShortcutCommandActivation renamingFor: MyApp.
```
This annotation will be cached and will keep cmd+r in instance variable. 
If you will modify #renamingFor: method with new shorctut the all shortcut annotations should be updated. Special pragma ensures this logic:
```Smalltalk
CmdShortcutCommandActivation class>> renamingFor: anAnnotationUser
   <classAnnotationDependency>
   ^self by: $r meta for: anAnnotationUser 
```

### Manual registry update
There are cases where registry can not track of annotation updates.

For example nobody forbids to pass the value of class side variable to an annotation instance. Variable change in that case will not affect dependent annotations by default. And the cached annotation instance will be not in synch with variable value.

In such cases developers should invalidate the annotation cache manually using the following code in required places: 
```Smalltalk
ClassAnnotation resetCache
```

## Installation
```Smalltalk
Metacello new
  baseline: 'ClassAnnotation';
  repository: 'github://dionisiydk/ClassAnnotation';
  load
```

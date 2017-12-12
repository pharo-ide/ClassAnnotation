# ClassAnnotation
This package introduces class annotations. They are first class objects and package defines how to attach them to classes.

To create a new annotation just create a subclass of ClassAnnotation:

```Smalltalk
ClassAnnotation subclass: #MySpecialAnnotation
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'MyPackage'
```
Then add new class side method to the class which you want to annotate. It should return an instance of the annotation. In addition, the method returning the annotation instance should be marked with pragma #classAnnotation:

```Smalltalk
MyClass class>>specialAnnotationExample
	<classAnnotation>
	
	^MySpecialAnnotation new
```

## Annotation queries
There are two ways how to query annotations:

1) You can ask concrete annotation class for all its registered instances:

```Smalltalk
MySpecialAnnotation registeredInstances
MySpecialAnnotation registeredInstancesAnnotating: MyClass
MySpecialAnnotation registeredInstancesDo: [:each | each logCr].
```

2) And you can ask a given class for all its attached annotations:

```Smalltalk
MyClass classAnnotations
MyClass classAnnotationsDo: [:each | each logCr]
```

Notice that each annotation includes the annotated class and the selector of declaration method. 

All annotations are cached in special registry. Therefore it is cheap to query them.

## Extending classes with meta information
Because annotations are declared in methods, they offer the interesting feature to extend meta information by external packages.

Just define declaration methods as class extensions, and when your package will be loaded the new annotations will be added to the existing class.

## Annotation instantiation
There is no special way how to instantiate annotation instances. It is up to your domain.

The base internal state of annotation is initialized during registry creation.  Users should not think about it. 

You can add any domain specific variables to your annotations and add constructors to initialize them in declaration methods. We give an example here after:



# Annotating Annotations
Annotations are just normal classes without any restrictions. You can also attach annotations to annotations like in other languages.

# Forbidden annotation
Annotation can forbid annotating of particular classes. For example it can forbid abstract classes.
```Smalltalk
MySpecificAnnotation>>isForbidden
      ^annotatedClass isAbstract
```
Such annotations will not be added to the registry and forbidden classes will not include them.

## Contextual annotations
Any annotation can be defined with context where it can be used. 
Imaging that you want annotate command classes with shortcuts for specific widgets:
```Smalltalk
MyCommand class>>widget1ShortcutAnnotation
   <classAnnotation>
   ^ShortcutAnnotation using: $r meta for: MyWidgetClass1

MyCommand class>> widget2ShortcutAnnotation
   <classAnnotation>
   ^ShortcutAnnotation using: $t meta for: MyWidgetClass2
```
Then you can query all shortcuts which should be active for concrete widget: 
```Smalltalk
ShortcutAnnotation activeAnnotationsInContext: aMyWidgetInstance
ShortcutAnnotation activeAnnotationsInContext: aMyWidgetInstance do: aBlock
```
Declaring context using classes is simplest case. Underhood class is converted to AnnotationContext instance using #asAnnotationContext message. Then during annotation query it simply asks #isKindOf: for given context instances.
For advanced scenarios you can implement more complex annotation context and define specific DSL to use them for annotations and queries.
 
Any annotation class can redefine meaning of active annotation with extra conditions. For example it can delegate decision to annotated class itself:
```Smalltalk
ShortcutAnnotation >>isActiveInContext: aMyWidgetInstance
   ^(super isActiveInContext: aMyWidgetInstance)
        and: [annotatedClass canBeUsedInWidget: aMyWidgetInstance]
```

But for some scenarios you may need to query annotations according to original "active" definition despite of extra conditions. For such cases the "visibility" of annotation is introduced: the annotation is visible if it is declared for given context:
```Smalltalk
ClassAnnotation>>isVisibleInContext: aContext
	^activeContext describes: aContext
```
So the visible annotation is not necessary active. But active annotation is always visible in given context:
```Smalltalk
ClassAnnotation>>isActiveInContext: aContext
	^self isVisibleInContext: aContext
```
Imaging that you want annotate commands with context menu information (where they should be accessible). In that case disabled menu items can represent commands which are visible for application but not active in given context (because selected items are not appropriate for them).
  
To query visible annotations there are few methods:
```Smalltalk
ContextMenuAnnotation visibleInstancesInContext: anUserContext
ContextMenuAnnotation visibleInstancesInContext: anUserContext do: aBlock
```

## Annotation priority
For particular scenarios it can be important to define order in which annotations are processed.
For context menu example this order can be used to sort menu items. 
For shortcut example it allows override existing shortcuts of application. 

So concept of annotation priority was introduced for this reason. Any annotation can define it. Annotation with greater priority value is processed first by enumeration methods:
- registeredInstancesDo:
- activeInstancesInContext:do:
- visibleInstancesInContext:do:

Priority is holden as instance variable. So you can specify it in declaration method:
```Smalltalk
MyCommand class>>shortcutAnnotation
   <classAnnotation>
   ^(ShortcutAnnotation using: $r meta for: MyWidgetClass) 
       priority: 1000
```
Of course for your annotation you can add instantiation methods which are suitable for your domain.

In some cases you would need override order in which annotations should be sorted. For example, you can say that command with greater priority should be at the end of menu.
For such cases you can override class side method #createContainerForRegistry:
```Smalltalk
MySpecificAnnotation class>>createContainerForRegistry
     ^SortedCollection sortBlock: #priority ascending
``` 

# Installation
```Smalltalk
Metacello new
  baseline: 'ClassAnnotation';
  repository: 'github://dionisiydk/ClassAnnotation';
  load
```

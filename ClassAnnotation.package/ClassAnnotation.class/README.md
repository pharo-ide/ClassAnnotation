I am the root of class annotation hierarchy.
My subclasses should annotate classes using class side methods with the pragma #classAnnotation. 
For example:

	MyClass class>>specialAnnotationExample
		<classAnnotation>
		^MySpecialAnnotation new

The annotating method should return an instance of the annotation.

I provide a query API to retrieve all registered instances of a concrete annotation class:

	MySpecialAnnotation registeredInstances.
	MySpecialAnnotation registeredInstancesFor: MyClass.
	MySpecialAnnotation registeredInstancesDo: [:each | each logCr].

Each annotation includes the annotated class and the selector of declaration method.
All annotations are cached in default ClassAnnotationRegistry instance. It is cheap to query them.

Classes itself can be queried for all attached annotations:

	MyClass classAnnotations.
	MyClass classAnnotationsDo: [:each | each logCr].

I provide extra hook to forbid annotating of particular classes. For example my subclasses can define that abstract classses should not be annotated by them.
The rule should be  implemented in the method:

	MySpecialAnnotation >>isForbidden
		^annotatedClass isAbstract 

By default method returns true which means that annotation can annotate any class.

Because annotations are declared in the methods it provides interesting feature to extend meta information from external packages.
Just define declaration method as class extension. And when your package will be loaded the new annotation will be added into existing class.
 
There is no special way how instantiate annotation instances. It is up to your domain.
The internal state is initialized during Registry creation.  Users should not think about it. 
You can add any domain specific variables to your annotations and add constructors to initialize them in declaration methods. 
Annotations are just normal objects without any restrictions. You can also attatch annotations to annotations like in other languages.

-----------Advanced features. Priority------------

Annotations in the registry are sorted by priority. It provides out of the box mechanizm for ordering annotated classes for the user domain purposes. 
Just pass priority number into the annotation when you create it in declaration method. And define suitable constructor for this.

-----------Advanced features. Context------------

Any annotation can be contextual. You can specify instance of context where annotation can be used:
	MySpecialAnnotation for: anAnotationContext
Context describes annotation users where they should be active.

For simplicity you can specify any class instead of context instance. It will represent all users of annotation of particular class hierarchy:

	MySpecialAnnotation for: MyUserClass.

Internallly argument is always converted to the context:

	MyUserClass asAnnotationContext.

I provide query interface to retriev registered annotations which are active in given context:

	MySpecialAnnotation activeInstancesInContext: anAnnotationUser.
	MySpecialAnnotation activeInstancesInContext: anAnnotationUser do: [:ann | ].
	MySpecialAnnotation activeInstancesFor: MyClass inContext: anAnnotationUser do: [:ann | ].

By default the annotation is active if given user is described by declared context:

	ClassAnnotation>>isActiveInContext: anAnnotationUser
		^activeContext describes: anAnnotationUser

Subclasses can provide extra conditions for active annotations. In that case they override this method:

	MySpecialAnnotation>>isActiveInContext: anAnnotationUser
		^(super isActiveInContext: anAnnotationUser)
			and: [annotatingClass canBeUsedInContext: anAnnotationUser]

So the logic can depends on annotating class itself and actual annotation user.

For some scenarios you may need to query annotations according to original "active" definition despite of extra conditions.
For such cases I introduced the "visibility" of annotations: the annotation is visible if it is declared for given user:

	ClassAnnotation>>isVisibleInContext: anAnnotationUser
		^activeContext describes: anAnnotationUser

So the visible annotation is not necessary active. But active annotation is always visible for given user:

	ClassAnnotation>>isActiveInContext: anAnnotationUser
		^self isVisibleInContext: anAnnotationUser

(I showed another version above to simplify description).
There are extra query methods to retrieve visible annotations:

	MySpecialAnnotation visibleInstancesInContext: anAnnotationUser.
	MySpecialAnnotation visibleInstancesInContext: anAnnotationUser do: [:ann | ].
	MySpecialAnnotation visibleInstancesFor: MyClass inContext: anAnnotationUser do: [:ann | ].

-----------Advanced features. Annotation dependency methods------------

It is possible to call other methods inside annotation declaring methods (with pragma <classAnnotation>).
Such methods are dependency methods and their modification requires updating annotation cache (registry).
You can do it manually using

	ClassAnnotation resetAll
	
Or you can mark such methods with special pragma <classAnnotationDependency> and systen will track these methods for automatically.	
For example in Commander package there is CmdShortcutCommandActivation annotation. It provides reusable methods for rename and remove shorcuts: cmd+r and cmd+x. So you can annotate commands using: 

	MyRenameCommand class>>shortcutActivation
		<classAnnotation>
		^CmdShortcutCommandActivation renamingFor: MyApp.

This annotation will keep cmd+r in instance variable. 
If you will modify #renamingFor: method with new shorctut the annotations should be updated. And special pragma ensures this logic:

	CmdShortcutCommandActivation class>>renamingFor: anAnnotationUser
		<classAnnotationDependency>
		^self by: $r meta for: anAnnotationUser 

-----------Advanced features. Redefining registered instances------------

All annotations are collected from methods and cached in default ClassAnnotationRegistry instance. 
I provide special mechanizm to redefine collected instances. When cache is updated I use and keep all redefined annotations.

To redefine particular annotation use #redefineBy: message with block which sets custom properties to original instance.
For example following code allows to redefine shortcut of #browse command in Calypso: 

	(ClySpawnFullBrowserCommand classAnnotationsAt: #browserShortcutActivation)
		redefineBy: [:shortcut | shortcut keyCombination: $o meta ].
	
Try evaluate it and press cmd+o on selected item in browser. If will open new browser window.
You can notice that old shortcut cmd+b is not working anymore.

Now you can manualy reset annotation cache to check that it will not affect redefined shortcut: 

	ClassAnnotation resetCache.
	
To inspect redefined annotations ask their class for: 

	CmdShortcutCommandActivation redefinedInstances
	
Redefined instances are stored in class side variable #redefinedInstances. 
It is a dictionary which keys are new redefining annotations and values are original annotations collected from methods.
Notice that key and value are equal objects because annotations define equality using annotated class and declaration selector.
So dictionary items can be accessed using both objects.

To check that annotation is redefined use following example: 

	 (ClySpawnFullBrowserCommand classAnnotationsAt: #browserShortcutActivation)
			isRedefined

And you can ask actual redefined annotaion: 

	 (ClySpawnFullBrowserCommand classAnnotationsAt: #browserShortcutActivation)
			redefinedInstance
		
Using annotation instance you can also retrieve redefining instance: 

	anAnnotation redefiningInstance.
	
It should be identical to cached one.
	
To revert redefined annotation use #revertRedefinedInstance message: 

	(ClySpawnFullBrowserCommand classAnnotationsAt: #browserShortcutActivation)
			revertRedefinedInstance

Check that now browse command is again activated by cmd+b shortcut (which is defined in annotation declaration method).

To revert all annotations use following script: 

	CmdShortcutCommandActivation revertRedefinedInstances

Redefining logic is very suitable mehanizm to override system behavior which depends on annotations without changing the code.
It can be used to manage particular kind of annotation in settings browser. 
For example shortcut annotations based on Commander are available in setting browser. Users can explore and edit all shortcuts in the system. And these settings are persistable.

-----------Internal Representation and Key Implementation Points------------
 
    Instance Variables
	annotatedClass:		<Class>
	declarationSelector:		<Symbol>
	priority:		<Number>
	activeContext:		<AnnotationContext>
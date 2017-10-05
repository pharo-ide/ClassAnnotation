I am the root of class annotation hierarchy.
My subclasses should annotate classes using class side methods with the pragma #classAnnotation. 
For example:
	MyClass class>>specialAnnotationExample
		<classAnnotation>
		^MySpecialAnnotation new
The annotating method should return an instance of the annotation.

I provide a query API to retrieve all registered instances of a concrete annotation class:
	MySpecialAnnotation registeredInstances
	MySpecialAnnotation registeredInstancesFor: MyClass
	MySpecialAnnotation registeredInstancesDo: [:each | each logCr].
Each annotation includes the annotated class and the selector of declaration method.
All annotations are cached in my Registry class var. It is cheap to query them.

Classes itself can be queried for all attached annotations:
	MyClass classAnnotations
	MyClass classAnnotationsDo: [:each | each logCr]

Because annotations are declared in the methods it provides interesting feature to extend meta information from external packages.
Just define declaration method as class extension. And when your package will be loaded the new annotation will be added into existing class.
 
There is no special way how instantiate annotation instances. It is up to your domain.
The internal state is initialized during Registry creation.  Users should not think about it. 
You can add any domain specific variables to your annotations and add constructors to initialize them in declaration methods. 
Annotations are just normal objects without any restrictions. You can also attatch annotations to annotations like in other languages.

Any annotation can be contextual. You can specify instance of context where annotation can be used:
	MySpecialAnnotation for: anAnotationContext
For simplicity you can specify any class as argument. It will represent any context of your application which is a kind of that class:
	MySpecialAnnotation for: MyContext
Internallly argument is always converted to the context:
	MyContext asAnnotationContext
I provide query interface to retrieve registered annotations which are active in given context:
	MySpecialAnnotation activeInstancesInContext: anUserContext
	MySpecialAnnotation activeInstancesInContext: anUserContext do: [:ann | ]
	MySpecialAnnotation activeInstancesFor: MyClass inContext: anUserContext do: [:ann | ]
By default the annotation is active in given context if it is described by declared context:
	ClassAnnotation>>isActiveInContext: aContext
		^activeContext describes: aContext
Subclasses can provide extra conditions for active annotations. In that case they override this method:
	MySpecialAnnotation>>isActiveInContext: aContext
		^(super isActiveInContext: aContext)
			and: [annotatingClass canBeUsedInContext: aContext]
So the logic can depends on annotating class itself and given actual context of annotation user.

For some scenarios you may need to query annotations according to original "active" definition despite of extra conditions.
For such cases I introduced  the "visibility" of annotations: the annotation is visible if it is declared for given context:
	ClassAnnotation>>isVisibleInContext: aContext
		^activeContext describes: aContext
So the visible annotation is not necessary active. But active annotation is always visible in given context:
	ClassAnnotation>>isActiveInContext: aContext
		^self isVisibleInContext: aContext
(I showed another version above to simplify description).
I provide extra query methods to retrieve visible annotations:
	MySpecialAnnotation visibleInstancesInContext: anUserContext
	MySpecialAnnotation visibleInstancesInContext: anUserContext do: [:ann | ]
	MySpecialAnnotation visibleInstancesFor: MyClass inContext: anUserContext do: [:ann | ]

    Instance Variables
	annotatedClass:		<Class>
	declarationSelector:		<Symbol>
	activeContext:		<AnnotationContext>
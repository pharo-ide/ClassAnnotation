I am the root class annotations hierarchy.
My subclasses should annotate classes using class side methods with the pragma #classAnnotation. 
For example:
	MyClass class>>specialAnnotationExample
		<classAnnotation>
		^MySpecialAnnotation new
The declaration method should return an instance of the annotation.

I provide a query API to retrieve all declared instances of a concrete annotation class:
	MySpecialAnnotation registeredInstances
	MySpecialAnnotation registeredInstancesAnnotating: MyClass
	MySpecialAnnotation registeredInstancesDo: [:each | each logCr].
Each annotation includes the annotated class and the selector of declaration method.
All annotations are cached in my Registry class var. It is cheap to query them.

Classes itself can be queried for all attached annotations:
	MyClass metaAnnotations
	MyClass metaAnnotationsDo: [:each | each logCr]

Because annotations are declared in the methods it provides interesting feature to extend meta information from external packages.
Just define declaration method as class extension. And when your package will be loaded the new annotation will be added into existing class.
 
There is no special way how instantiate annotation instances. It is up to your domain.
My own internal state is initialized during Registry creation.  Users should not think about it. 
You can add any domain specific variables to your annotations and add constructors to initialize them in declaration methods. 
Annotations are just normal objects without any restrictions. You can also attatch annotations to annotations like in other languages

    Instance Variables
	annotatedClass:		<Class>
	declarationSelector:		<Symbol>
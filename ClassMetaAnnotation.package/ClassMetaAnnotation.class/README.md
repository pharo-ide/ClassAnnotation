I represent first class meta annotation which can be attached to classes using class side methods with the pragma #classMetaAnnotation. 
For example:
	MyClass class>>specialAnnotationExample
		<classMetaAnnotation>
		^MySpecialAnnotation new
The declaration method should return an instance of the annotation.

I provide a query API to retrieve all declared instances of a concrete annotation class:
	MySpecialAnnotation declaredInstances
	MySpecialAnnotation declaredInstancesFor: MyClass
	MySpecialAnnotation declaredInstancesDo: [:each | each logCr].
All annotations are cached in my Registry class var. It is cheap to query them.

I extend Class with new methods to retrieve all attached annotations:
	MyClass metaAnnotations
	MyClass metaAnnotationsDo: [:each | each logCr]

Because annotations are declared in the methods it provides interesting feature to extends meta information from external packages.
Just define declaration method as class extension. And when your package will be loaded the new annotation will be added into existing class.
 
There is no special way how instantiate annotation instances. It is up to your domain.
My own internal state is initialized during Registry creation.  Users should not think about it. 
You can add any domain specific variables to your annotations and add constructors to initialize them in declaration methods. 
Annotations are just normal objects without any restrictions. You can also attatch annotations to annotations like in other languages

    Instance Variables
	annotatedClass:		<Class>
	declarationSelector:		<Symbol>
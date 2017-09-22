# ClassMetaAnnotation
This package implements first class meta annotations which can be attached to classes using class side methods with the pragma #classMetaAnnotation. 

For example:
```Smalltalk
MyClass class>>specialAnnotationExample
	<classMetaAnnotation>
	
	^MySpecialAnnotation new
```
The declaration method should return an instance of the annotation.

## Annotation queries
There are two type of queries.

You can query all declared instances of concrete annotation class:
```Smalltalk
MySpecialAnnotation declaredInstances
MySpecialAnnotation declaredInstancesFor: MyClass
MySpecialAnnotation declaredInstancesDo: [:each | each logCr].
```
And you can query a class for all attached annotations:
```Smalltalk
MyClass metaAnnotations
MyClass metaAnnotationsDo: [:each | each logCr]
```
Notice that each annotation includes the annotated class and the selector of declaration method. 

All annotations are cached in the special registry. It is cheap to query them.

## Extending classes with meta information
Because annotations are declared in methods it provides interesting feature to extend meta information by external packages.

Just define declaration method as class extension. And when your package will be loaded the new annotation will be added into existing class.

## Annotation instantiation
There is no special way how instantiate annotation instances. It is up to the domain.

The base internal state of annotation is initialized during Registry creation.  Users should not think about it. 

You can add any domain specific variables to your annotations and add constructors to initialize them in declaration methods. 

Annotations are just normal objects without any restrictions. You can also attatch annotations to annotations like in other languages

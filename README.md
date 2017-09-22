# ClassMetaAnnotation
This package provides the way how implement first class meta annotations and how to attach them to classes.

To create new annotation just subclass ClassMetaAnnotation:
```Smalltalk
ClassMetaAnnotation subclass: #MySpecialAnnotation
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'MyPackage'
```
Then add new method to the class which you want annotate. It should be class side method and it should return the instance of annotation class. In addition the method should be marked with pragma #classMetaAnnotation:
```Smalltalk
MyClass class>>specialAnnotationExample
	<classMetaAnnotation>
	
	^MySpecialAnnotation new
```

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
There is no special way how instantiate annotation instances. It is up to your domain.

The base internal state of annotation is initialized during registry creation.  Users should not think about it. 

You can add any domain specific variables to your annotations and add constructors to initialize them in declaration methods. 

# Annotating Annotations
Annotations are just normal classes without any restrictions. You can also attatch annotations to annotations like in other languages

# Installation
```Smalltalk
Metacello new
  baseline: 'ClassMetaAnnotation';
  repository: 'github://dionisiydk/ClassMetaAnnotation';
  load
```

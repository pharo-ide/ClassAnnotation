# ClassAnnotation
This package provides the way how implement first class annotations and how to attach them to classes.

To create new annotation just subclass ClassAnnotation:
```Smalltalk
ClassAnnotation subclass: #MySpecialAnnotation
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'MyPackage'
```
Then add new class side method to the class which you want annotate. It should return an instance of the annotation. And the method should be marked with pragma #classAnnotation:
```Smalltalk
MyClass class>>specialAnnotationExample
	<classAnnotation>
	
	^MySpecialAnnotation new
```

## Annotation queries
There are two ways how to query annotations:

1) You can ask concrete annotation class for all registered instances:
```Smalltalk
MySpecialAnnotation registeredInstances
MySpecialAnnotation registeredInstancesAnnotating: MyClass
MySpecialAnnotation registeredInstancesDo: [:each | each logCr].
```
2) And you can ask given class for all attached annotations:
```Smalltalk
MyClass metaAnnotations
MyClass metaAnnotationsDo: [:each | each logCr]
```
Notice that each annotation includes the annotated class and the selector of declaration method. 

All annotations are cached in special registry. It is cheap to query them.

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
  baseline: 'ClassAnnotation';
  repository: 'github://dionisiydk/ClassAnnotation';
  load
```

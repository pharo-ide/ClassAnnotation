# ClassAnnotation
This package introduces clas annotations. They are first class objects and package defines how to attach them to classes.

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

# Installation
```Smalltalk
Metacello new
  baseline: 'ClassAnnotation';
  repository: 'github://dionisiydk/ClassAnnotation';
  load
```

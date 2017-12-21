I represent any annotation user from particular class hierarchy.

I am hidden from programmers. They just specify the class of annotation user when they create annotations:
	MyClassAnnotation for: MyUserClass
Internally given class is converted to my instance:
	MyUserClass asAnnotationContext

When annotations are queried for particular user instance the result will include all annotations defined for given user class and its subclasses.	
I provide little hook to redefine this rule by annotation user. Instead of simple isKindOf check:
	anAnnotationUser isKindOf: annotationUserClass 
I ask given user if it represents simple annotation user:
	anAnnotationUser representsSimpleAnnotationUser: annotationUserClass
And by default objects just performs isKindOf: check to implement it.

To create my instance manually use:
	SimpleAnnotationContext of: MyUserClass

I define comparison methods in the way that two contexts with same user class will be equal.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	annotationUserClass:		<Class>
I represent hierarchy of contexts where annotation can be used.

I am hidden from users. They just specify the class of context when they create annotations:
	MyClassAnnotation for: MyContext
And I am created from the given argument using convertation method:
	MyContext asAnnotationContext

When users query annotations in given context the result will include all annotations defined for given context class and its subclasses.	

To create my instance manually use:
	SimpleAnnotationContext userContextClass: MyContext

I define comparison methods in the way that two context with same class will be equal.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	userContextClass:		<Class>
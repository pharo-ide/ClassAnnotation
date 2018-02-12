I implement a registry of class annotations.
I organize it in two dictionaries:

1)  annotations
It maps annotation class to all instances of this class. Instances are sorted by annotation priority in ascending order. 
Annotation class is responsible to provide such container: 

	ClassAnnotation createContainerForRegistry

For details about annotation priorities look at ClassAnnotation comment.

2) annotatedClasses
It maps annotated class to all its annotations. The annotations are managed as a Set.

I provide default instance which keeps all class annotations in the system.

	ClassAnnotationRegistry default
	
 It is used by ClassAnnotation query methods and you can access it from it:
	
	ClassAnnotation registry

I am subscribed on system changes and reset default instance when related methods or classes are changed.
And when user access annotations I rebuild it lazily. 
Look for details in class side method #ensureSystemSubscription  

Internal Representation and Key Implementation Points.

    Instance Variables
	annotations:		<IdentityDictionary<ClassAnnotation class, SortedCollection<ClassAnnotation>>>
	annotatedClasses:		<IdentityDictionary<Class, Set<ClassAnnotation>>>
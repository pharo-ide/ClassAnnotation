My subclasses represent context of annotation where it can be used.
They should implement single method #describes: with context as argument. Context can be any object. It is provided by domain users of annotation.

By default annotations has NullAnnotationContext which not describes any possible context. So when you query annotations in given context then annotations without specified contexts will be skipped.

For simplicity there is on simple context implementation SimpleAnnotationContext which is defined by class. And it represent any context, a kind of given class.

It can be created directly from class using convertation method:
	MyContext asAnnotationContext.
So you can create annotations using class of context:
	MyClassAnnotation for: MyContext

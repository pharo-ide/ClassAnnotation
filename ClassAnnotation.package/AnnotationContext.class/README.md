My subclasses represent context of annotation where it can be used.
They should implement single method #describes: with context as argument. Context can be any object. It is provided by domain users of annotation.

By default annotations has NullAnnotationContext which not describes any possible context. So when you query annotations in given context then annotations without specified contexts will be skipped.

For simplicity there is one simple context implementation the SimpleAnnotationContext which represents the hierarchy of given context class. It is hidden from users and created when class is specified for annotation context:
	MyClassAnnotation for: MyContext
Look at subclasse comments for details
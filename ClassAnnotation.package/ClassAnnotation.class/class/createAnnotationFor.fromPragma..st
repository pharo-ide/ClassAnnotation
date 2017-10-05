private
createAnnotationFor: aClass fromPragma: aPragma
	"Here we evaluate pragma method expecting annotation instance as result.
	Of course the method can fail or it can not return annotation 
	(user can forgot return for example). 
	In such cases BrokenClassAnnotation will be used instead.
	Also here we perform check isForbidden which should be evaluated on initialized instance 
	and also can fail due to bad implementation. 
	Failed check will also lead to BrokenClassAnnotation instance.
	If annotation instance is forbidden 
	then the ForbiddenClassAnnotation instance will be returned instead.
	And it will will not be added to the registry"
	| annotation |
	[
		annotation := aClass perform: aPragma methodSelector.
		(annotation isKindOf: ClassAnnotation) ifFalse: [ 
			annotation := BrokenClassAnnotation forBadMethodReturn].
		"isForbidden should be performed with initialized instance"
		annotation 
			annotatedClass: aClass;
			declarationSelector: aPragma methodSelector.
		annotation isForbidden ifTrue: [ annotation := ForbiddenClassAnnotation new ]
	] on: Error do: [:err | 
		annotation := BrokenClassAnnotation withError: err].
	"For simplisity initialization is repeated although it can be already initialized"
	^annotation 
		annotatedClass: aClass;
		declarationSelector: aPragma methodSelector
private
createAnnotationFor: aClass fromPragma: aPragma
	
	| annotation |
	annotation := [aClass perform: aPragma methodSelector] on: Error do: [:err | 
		BrokenClassAnnotation withError: err].
	(annotation isKindOf: ClassAnnotation) ifFalse: [ 
		annotation := BrokenClassAnnotation forBadMethodReturn].
	annotation 
		annotatedClass: aClass;
		declarationSelector: aPragma methodSelector.
	^annotation
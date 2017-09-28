private
createAnnotationFor: aClass fromPragma: aPragma
	
	| annotation |
	annotation := [aClass perform: aPragma methodSelector] on: Error do: [:err | 
		BrokenClassAnnotation withError: err].
	annotation 
		annotatedClass: aClass;
		declarationSelector: aPragma methodSelector.
	^annotation
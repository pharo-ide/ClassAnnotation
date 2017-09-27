private
createAnnotationFor: aClass fromPragma: aPragma
	
	| annotation |
	annotation := [aClass perform: aPragma methodSelector] on: Error do: [:err | 
		BrokenClassMetaAnnotation withError: err].
	annotation 
		annotatedClass: aClass;
		declarationSelector: aPragma methodSelector.
	^annotation
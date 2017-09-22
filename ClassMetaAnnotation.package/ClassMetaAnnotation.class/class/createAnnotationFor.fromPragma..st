private
createAnnotationFor: aClass fromPragma: aPragma
	
	| annotation |
	annotation := aClass perform: aPragma methodSelector.
	annotation 
		annotatedClass: aClass;
		declarationSelector: aPragma methodSelector.
	^annotation
	
testing
isActiveInContext: aContext
	"The annotation can be active only when it is visible in given context.
	But other conditions can be required for concrete type of annotation.
	For example the activeness of the annotation can depends on the state of given context.
	In that case annotation should override this method with extra logic.
	But by default visible annotation is also active"
	
	^self isVisibleInContext: aContext
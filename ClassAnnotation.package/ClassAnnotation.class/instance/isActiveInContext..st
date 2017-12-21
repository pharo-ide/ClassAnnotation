testing
isActiveInContext: anAnnotationUser
	"The annotation can be active only when it is visible for given user.
	But other conditions can be required for concrete type of annotation.
	For example the activeness of the annotation can depends on the state of given user.
	In that case annotation should override this method with extra logic.
	But by default visible annotation is also active"
	
	^self isVisibleInContext: anAnnotationUser
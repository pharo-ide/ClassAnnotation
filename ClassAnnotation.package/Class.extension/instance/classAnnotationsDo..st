*ClassAnnotation
classAnnotationsDo: aBlock
	^(ClassAnnotation findAllAnnotationsFor: self) do: aBlock
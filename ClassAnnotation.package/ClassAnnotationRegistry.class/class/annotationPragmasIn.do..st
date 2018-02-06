pragma collecting
annotationPragmasIn: aClass do: aBlock

	Pragma withPragmasIn: aClass do: [ :pragma |
		pragma keyword == #classAnnotation ifTrue: [aBlock value: pragma]]
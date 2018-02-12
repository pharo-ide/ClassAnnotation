pragma collecting
annotationPragmasIn: aClass do: aBlock

	Pragma withPragmasIn: aClass do: [ :pragma |
		pragma keyword == self annotationPragmaName ifTrue: [aBlock value: pragma]]
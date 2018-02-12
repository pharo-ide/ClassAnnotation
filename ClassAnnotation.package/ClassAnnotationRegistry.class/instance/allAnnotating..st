queries
allAnnotating: aClass

	^annotatedClasses at: aClass ifAbsent: [ #() ]
private
collectAllDeclaredInstances

	| result declaredInstances annotations |
	result := IdentityDictionary new.
	self declarationPragmaCollector reset.
	self declarationPragmaCollector do: [ :pragma |
		annotations := self createAnnotationsForAllSubclassesFromPragma: pragma.
		annotations do: [ :each | 
			declaredInstances := result at: each class ifAbsentPut: [each class createContainerForRegistry].
			(declaredInstances includes: each) ifFalse: [ 
				"This check avoids dublicated instances from overriden methods"
				declaredInstances add: each]]].
	
	^result
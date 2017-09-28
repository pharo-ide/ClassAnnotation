private
collectAllDeclaredInstances

	| result declaredInstances annotations |
	result := IdentityDictionary new.
	self declarationPragmaCollector reset.
	self declarationPragmaCollector do: [ :pragma |
		annotations := self createAnnotationsForAllSubclassesFromPragma: pragma.
		annotations do: [ :each | 
			declaredInstances := result at: each class ifAbsentPut: [ Set new ].
			declaredInstances add: each]].
	
	^result
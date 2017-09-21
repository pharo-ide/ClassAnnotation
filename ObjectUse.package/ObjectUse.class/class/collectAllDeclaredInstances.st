private
collectAllDeclaredInstances

	| result use declaredInstances |
	result := IdentityDictionary new.
	
	self declarationPragmaCollector do: [ :pragma |
		use := pragma methodClass instanceSide perform: pragma methodSelector.
		declaredInstances := result at: use class ifAbsentPut: [ OrderedCollection new ].
		declaredInstances add: use].
	
	^result
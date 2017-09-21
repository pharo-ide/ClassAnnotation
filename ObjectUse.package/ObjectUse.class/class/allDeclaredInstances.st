private
allDeclaredInstances

	^AllDeclaredInstances ifNil: [ AllDeclaredInstances := self collectAllDeclaredInstances ]
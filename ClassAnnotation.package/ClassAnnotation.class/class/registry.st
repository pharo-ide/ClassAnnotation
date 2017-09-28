private
registry

	^Registry ifNil: [ Registry := self collectAllDeclaredInstances ]
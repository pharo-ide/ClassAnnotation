private
declarationPragmaCollector

	^DeclarationPragmaCollector ifNil: [ 
		DeclarationPragmaCollector := self createPragmaCollector]
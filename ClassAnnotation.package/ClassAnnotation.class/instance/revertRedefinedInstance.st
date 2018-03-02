redefining
revertRedefinedInstance

	^self revertRedefinedInstanceIfAbsent: [
		self error: 'Redefined annotation not exists anymore!']
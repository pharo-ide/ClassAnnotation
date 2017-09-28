tests
testDeclaredInstancesEnumeration

	| enumerated |
	enumerated := OrderedCollection new.
	ClassAnnotationExample1 declaredInstancesDo: [:each | enumerated add: each].
	
	self assert: enumerated asArray equals: ClassAnnotationExample1 declaredInstances asArray
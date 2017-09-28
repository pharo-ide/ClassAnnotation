tests
testEnumeratingAnnotationsOfGivenClass

	| enumerated |
	enumerated := OrderedCollection new.
	ClassWithThreeMetaAnnotations metaAnnotationsDo: [:each | enumerated add: each].
	
	self assert: enumerated asArray equals: ClassWithThreeMetaAnnotations metaAnnotations asArray
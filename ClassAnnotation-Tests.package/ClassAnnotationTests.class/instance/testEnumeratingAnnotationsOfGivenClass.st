tests
testEnumeratingAnnotationsOfGivenClass

	| enumerated |
	enumerated := OrderedCollection new.
	ClassWithThreeAnnotations metaAnnotationsDo: [:each | enumerated add: each].
	
	self assert: enumerated asArray equals: ClassWithThreeAnnotations metaAnnotations asArray
tests
testEnumeratingAnnotationsOfGivenClass

	| enumerated |
	enumerated := OrderedCollection new.
	ClassWithThreeAnnotations classAnnotationsDo: [:each | enumerated add: each].
	
	self assert: enumerated asArray equals: ClassWithThreeAnnotations classAnnotations asArray
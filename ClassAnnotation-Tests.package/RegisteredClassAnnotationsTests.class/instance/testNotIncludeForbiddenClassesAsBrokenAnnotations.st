tests
testNotIncludeForbiddenClassesAsBrokenAnnotations
	| broken |
	
	broken := BrokenClassAnnotation registeredInstancesFor: ClassWithForbiddenAnnotationExample.
	broken 
		detect: [:each | each declarationSelector = #forbiddenAnnotation]
		ifFound: [ :ann | self assert: false description: 'forbidden annotation should not be broken' ]
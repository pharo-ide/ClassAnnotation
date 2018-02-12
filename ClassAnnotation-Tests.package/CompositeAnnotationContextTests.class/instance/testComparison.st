tests
testComparison
	
	self 
		assert: Object asAnnotationContext, Array asAnnotationContext 
		equals: Object asAnnotationContext, Array asAnnotationContext. 
	self 
		assert: (Object asAnnotationContext, Array asAnnotationContext) hash 
		equals: (Object asAnnotationContext, Array asAnnotationContext) hash.
	
	self deny: (Object asAnnotationContext, Array asAnnotationContext)
						=  (Object asAnnotationContext, String asAnnotationContext)
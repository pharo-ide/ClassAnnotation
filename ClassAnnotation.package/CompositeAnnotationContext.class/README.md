I am composite context. I represent users of annotations which satisfy all my parts.

To create my instances use following method:

	CompositeAnnotationContext with: {aContext1. aContext2}
	
Or use comma message to concatenate other contexts: 

	aContext1 , aContext2

Internal Representation and Key Implementation Points.

    Instance Variables
	parts:		<Collection of<AnnotationContext>>
I represent broken class annotation when declaration method raises error during registry building.

I include description of raisen error in my variable errorString.

To debug broken annotation you can inspect all of them using:
	BrokenClassMetaAnnotation declaredInstances 
Then ask #debug to choosen annotation:
	brokenAnnotation debug.
It perform declaration method which will open debugger.	

My instances are created using mesage #withError:
	BrokenClassMetaAnnotation withError: anError
I only collect description of given error to not keep garbage of failed process.	

Internal Representation and Key Implementation Points.

    Instance Variables
	errorString:		<String>

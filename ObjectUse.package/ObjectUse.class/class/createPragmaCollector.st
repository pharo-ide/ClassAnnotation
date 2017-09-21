private
createPragmaCollector

	| collector |
	collector := PragmaCollector filter: [ :pragma | pragma keyword == #objectuse].
	collector reset.
	collector whenChangedSend: #reset to: self.	
	^collector
baselines
baseline: spec
	<baseline>
	spec for: #'common' do: [
		spec 
			package: 'ClassMetaAnnotation';
			package: 'ClassMetaAnnotation-Tests' with: [spec requires: #(ClassMetaAnnotation)].
		spec 
			group: 'default' with: #('Core' 'Tests');
			group: 'Core' with: #('ClassMetaAnnotation');
			group: 'Tests' with: #('ClassMetaAnnotation-Tests') ].

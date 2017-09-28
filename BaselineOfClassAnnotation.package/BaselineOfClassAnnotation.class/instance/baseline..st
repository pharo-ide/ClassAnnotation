baselines
baseline: spec
	<baseline>
	spec for: #'common' do: [
		spec 
			package: 'ClassAnnotation';
			package: 'ClassAnnotation-Tests' with: [spec requires: #(ClassAnnotation)].
		spec 
			group: 'default' with: #('Core' 'Tests');
			group: 'Core' with: #('ClassAnnotation');
			group: 'Tests' with: #('ClassAnnotation-Tests') ]
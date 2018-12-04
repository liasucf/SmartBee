package evolutionEnum;

public enum EvolutionEnum {
	APPEARS(1), DISAPPEARS(2), SHRINKS(3), EXPANDS(4), MERGE(5), SPLIT(6), SURVIVES(7), ROOT(0);
	
	private int value;
	
	private EvolutionEnum(int value){
		this.setValue(value);
	}

	public int getValue() {
		return value;
	}

	public void setValue(int value) {
		this.value = value;
	}
}

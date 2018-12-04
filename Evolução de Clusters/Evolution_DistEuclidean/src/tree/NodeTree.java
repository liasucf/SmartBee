package tree;

import java.util.ArrayList;

import evolutionEnum.EvolutionEnum;

public class NodeTree {

	private EvolutionEnum evolution;
	private double idNode;
	private ArrayList<NodeTree> childNodes;
	public boolean isVisited;
	public int time;
	public static void main(String[] args) {}
	
	public NodeTree(EvolutionEnum evolution, double clusterj, int time){
		this.setEvolution(evolution);
		this.setIdNode(clusterj);
		childNodes = new ArrayList<NodeTree>();
		isVisited = false;
		this.time=time;
	}
	
	public ArrayList<NodeTree> getChildNodes(){
		return childNodes;
	}
	
	public void addChildNode(NodeTree child){
		childNodes.add(child);
	}

	public double getIdNode() {
		return idNode;
	}

	public void setIdNode(double clusterj) {
		this.idNode = clusterj;
	}

	public EvolutionEnum getEvolution() {
		return evolution;
	}

	public void setEvolution(EvolutionEnum evolution) {
		this.evolution = evolution;
	}
}

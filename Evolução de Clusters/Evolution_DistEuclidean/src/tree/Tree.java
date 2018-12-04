package tree;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Stack;

import evolutionEnum.EvolutionEnum;

public class Tree{
//HashMap is a Map based collection class that is
//used for storing Key & value pairs, it is denoted as HashMap<Key, Value>
	private HashMap<Double, NodeTree> hashEvolution;
	private NodeTree root;
	private HashMap<NodeTree,ArrayList<ArrayList<NodeTree>>> result;
	//TODO registra a sequencia de clusters que eh frequente
	private FileWriter fwResult;
//write () método de BufferedWriter para gravar o texto em um arquivo. A vantagem de usar o BufferedWriter é que ele grava texto em um fluxo de saída de caracteres, armazenando em buffer caracteres para 
//fornecer a gravação eficiente (melhor desempenho) de caracteres únicos, matrizes e cadeias de caracteres.
	BufferedWriter bwResult;
	private int k;
	
	public Tree(NodeTree root) {
		k=1;
		setHashEvolution(new HashMap<Double,NodeTree>());
		this.root = root;
	}

	public NodeTree getRoot() {
		return root;
	}

	public void setRoot(NodeTree root) {
		this.root = root;
	}

	public HashMap<Double, NodeTree> getHashEvolution() {
		return hashEvolution;
	}


	public void setHashEvolution(HashMap<Double, NodeTree> hashEvolution) {
		this.hashEvolution = hashEvolution;
	}
	
	public void printTree(){
		Stack<NodeTree> stack = new Stack<>();
		NodeTree nodeAux;
		ArrayList<NodeTree> visited = new ArrayList<NodeTree>(); 
		
		stack.push(root);	
		//se a arvore nao tiver vazia voce, voce pega uma folha dela e ver se ela n foi visitada ainda 
		//se nao foi, voce visita adcionando ela ao array visited
		while(!stack.empty()){
			nodeAux = stack.pop();
			if(!visited.contains(nodeAux)){
				visited.add(nodeAux);
				for (NodeTree nodeTree : nodeAux.getChildNodes()) {
					stack.push(nodeTree);
					System.out.println("From:"+nodeAux.getIdNode()+" transition:"+nodeAux.getEvolution().toString()+" "
							+ "To:"+nodeTree.getIdNode()+" transition:"+nodeTree.getEvolution().toString());
				}
			}
		}
	}
	
	public void printTreeBFS()    
	{      
		 ArrayList<NodeTree> q = new ArrayList<NodeTree>();
		 q.add(root);
		 while (q.size() > 0)
		 {
		    NodeTree n = q.remove(0);
		    System.out.println(n.getEvolution().toString());
		    for (NodeTree nodeTree : n.getChildNodes()) {
				q.add(nodeTree);
			}
		 }
	}
	
	public void printAllRootToLeafPaths(NodeTree node,ArrayList<NodeTree> path) 
	{	//node é a raiz
	    if(node==null){
	        return;
	    }
	    path.add(node);
	    //se tiver chegado na folha significa que todos os nos estao adcionados no path e ja pode printar o path
	    if(node.getChildNodes().isEmpty())
	    {
	       for (int i = 0; i < path.size(); i++) {
			System.out.print("--"+path.get(i).getEvolution().toString()+" "+path.get(i).getIdNode());
	       }
	       System.out.println();
	        return;
	    }
	    else
	    {
	    	for (NodeTree nodeTree : node.getChildNodes()) {
	    		printAllRootToLeafPaths(nodeTree,new ArrayList<NodeTree>(path));
			}
	    }      
	}
	
	/*public int countSubgraphsIsomorphous(ArrayList<EvolutionEnum> sequencePattern){
		setNotVisited();
		ArrayDeque<NodeTree> queue = new ArrayDeque<NodeTree>();
		HashMap<NodeTree, ArrayList<Double>> hashAux = new HashMap<NodeTree,ArrayList<Double>>();
		NodeTree nodeAux;
		int count=0;

		//adiciono a raiz na fila
		queue.add(root);
		root.isVisited=true;
		//se a raiz satisfaz o primeiro elemento da sequencia de padroes
		if(root.getEvolution()==sequencePattern.get(0)){
			hashAux.put(root, new ArrayList<Double>());
			hashAux.get(root).add(0);
		}
		
		while(!queue.isEmpty()){
			nodeAux = queue.remove();
			
			//no faz parte da sequencia buscada
			if(hashAux.containsKey(nodeAux)){
				//em quais posicoes na sequencia o no que estah sendo examinado correponde ao mesmo padrao de evolucao
				for (int posSequence : hashAux.get(nodeAux)) {
					//verificar se a posicao na sequencia eh a ultima
					if(posSequence==sequencePattern.size()-1){
						count++;
						addChildToHash(nodeAux, sequencePattern, hashAux,0, queue);						
					}//end if
					else{
						addChildToHash(nodeAux, sequencePattern, hashAux, posSequence+1, queue);
					}//end else
				}//end for
				
				//remove no hash o noh nodeAux, pois jah foi examinado
				hashAux.remove(nodeAux);
				
			}//end if
			else{
				addChildToHash(nodeAux, sequencePattern, hashAux,0, queue);
			}//end else
		}		
		return count;
	}
	
	public void addChildToHash(NodeTree nodeAux, ArrayList<EvolutionEnum> sequencePattern,HashMap<NodeTree, ArrayList<Double>> hashAux, int posToCheck,ArrayDeque<NodeTree> queue){
		ArrayList<Double> arrayNodeAux = null;
				
		//caso o noh pai nao tenha sequencia de evolucao semelhante a buscada 
		for (NodeTree nodeAuxChild : nodeAux.getChildNodes()) {
			arrayNodeAux = null;
			//para os nos filhos ainda nao visitados
			if(!nodeAuxChild.isVisited)	{
				nodeAuxChild.isVisited=true;
				queue.add(nodeAuxChild);
			}
			else arrayNodeAux = hashAux.get(nodeAuxChild);
			//no filho tem evolucao tem evolucao semelhante a primeira evolucao buscada 
			if(nodeAuxChild.getEvolution()==sequencePattern.get(posToCheck)){
				if(arrayNodeAux==null){
					arrayNodeAux = new ArrayList<Double>();
				}
				//a partir desse no visitado pode haver uma nova sequencia semelhante a buscada
				if(posToCheck!=0 && nodeAuxChild.getEvolution()==sequencePattern.get(0)) arrayNodeAux.add(0);
				arrayNodeAux.add(posToCheck);
								
				hashAux.put(nodeAuxChild, arrayNodeAux);
			}
		
		}//end for
	}*/
	
	//TODO para retornar os clusters
	public int countSubgraphsIsomorphous(ArrayList<EvolutionEnum> sequencePattern, int support) throws IOException{
		//lista de nodeTree auxiliar.
		ArrayList<NodeTree> resultAux;
		
		//os clusters que estao nos nohs da arvore, vao formando um subgrafo frequente
		result = new HashMap<NodeTree,ArrayList<ArrayList<NodeTree>>>();
		
		setNotVisited();
		//usado no percurso em arvore
		ArrayDeque<NodeTree> queue = new ArrayDeque<NodeTree>();
		//para cada no, armazena as posicoes no sequencePattern que casam com esse noh
		HashMap<NodeTree, ArrayList<Double>> hashAux = new HashMap<NodeTree,ArrayList<Double>>();
		NodeTree nodeAux;
		int count=0;

		//adiciono a raiz na fila
		queue.add(root);
		root.isVisited=true;
		//se a raiz satisfaz o primeiro elemento da sequencia de padroes
		//se na sequencia de padroes passado o primeiro elemento for ROOT entao
		if(root.getEvolution()==sequencePattern.get(0)){
			hashAux.put(root, new ArrayList<Double>());
			hashAux.get(root).add((double) 0);
			
			//guarda o caminho frequente em que o ultimo noh pertencente ao subgrafo eh a raiz
			result.put(root, new ArrayList<ArrayList<NodeTree>>());
			resultAux = new ArrayList<NodeTree>();
			resultAux.add(root);
			result.get(root).add(resultAux);
		}
		
		while(!queue.isEmpty()){
			nodeAux = queue.remove();
			
			//no jah foi alcancado antes e pertence a sequencia
			//se o noh jah foi colocado na pilha, eh porque jah foi comparado ele e a sequencia buscada
			if(hashAux.containsKey(nodeAux)){

				//em quais posicoes na sequencia o no que estah sendo examinado correponde ao mesmo padrao de evolucao
				for (Double posSequence : hashAux.get(nodeAux)) {
					//verificar se a posicao na sequencia eh a ultima
					if(posSequence==sequencePattern.size()-1){
						count++;
						for (Double in : hashAux.get(nodeAux)) {
							System.out.println("ESTA CONTANDO PARA QUEM TERMINA:"+nodeAux.getIdNode()+" posicao "+in);
						}
						addChildToHash(nodeAux, sequencePattern, hashAux,0, queue);						
					}//end if
					else{
						addChildToHash(nodeAux, sequencePattern, hashAux, posSequence+1, queue);
					}//end else
				}//end for
				
				//remove no hash o noh nodeAux, pois jah foi examinado
				hashAux.remove(nodeAux);
				//TODO todo os resultados que tiverem tamanho diferente ao sequencePattern eh porque nao sao subgrafos de sequencePattern
				//result.remove(nodeAux);
			/*	ArrayList<Double> indexes = new ArrayList<Double>();
				ArrayList<ArrayList<NodeTree>> arrayNodeTree=result.get(nodeAux);
				for (int i = 0; i < arrayNodeTree.size(); i++) {
					if(arrayNodeTree.get(i).size()!=sequencePattern.size()) {
						indexes.add(i);
					}
				}
				for (Double i : indexes) {
					arrayNodeTree.remove(i);
				}
				
				
				if(!arrayNodeTree.isEmpty()){
					result.put(nodeAux, arrayNodeTree);
					System.out.println("COLOCOU NO NOH: "+nodeAux.getIdNode());
					for (ArrayList<NodeTree> arrayList : arrayNodeTree) {
						for (NodeTree nodeTree : arrayList) {
							System.out.println("Elementos que pertencem ao cluster:"+nodeTree.getIdNode());
						}
					}
				}
				*/
			}//end if
			else{
				addChildToHash(nodeAux, sequencePattern, hashAux,0, queue);
			}//end else
		}		
		
		if(count>=support){
			//TODO IMPRIMIR TODOS OS PONTOS DOS CLUSTER QUE ESTAO NOS CAMINHOS QUE CONTINUARAM EM RESULT
			k = sequencePattern.size();
			fwResult = new FileWriter("C:\\\\Users\\\\Lia\\\\Desktop\\\\Codigos- Evolução de Clusters\\clustersFrequentes"+k+".txt", true);
			bwResult = new BufferedWriter(fwResult);
			
			for (EvolutionEnum evolutionEnum : sequencePattern) {
				System.out.print(evolutionEnum+"--");
				bwResult.write(evolutionEnum+"--");
			}
			ArrayList<String> alreadyCopied = new ArrayList<String>();
			String stringAux;
			
			System.out.println();
			bwResult.newLine();
			
			for (NodeTree nodeTree : result.keySet()) {
				for (ArrayList<NodeTree> arrayList : result.get(nodeTree)) {
					if(arrayList.size()==sequencePattern.size()){
						stringAux = "";
						for (NodeTree n : arrayList) {
							stringAux+="Cluster id:"+n.getIdNode()+"  time: "+n.time+"\n";
						}
						boolean alreadyTested = false;
						for (int i = 0; i < alreadyCopied.size() && !alreadyTested; i++) {
							if(alreadyCopied.get(i).contentEquals(stringAux)) {
								alreadyTested=true;
							}
						}						
						if(!alreadyTested){
							System.out.println("---------------------------");
							System.out.println(stringAux);
							bwResult.write("---------------------------");
							bwResult.newLine();
							bwResult.write(stringAux);
							alreadyCopied.add(stringAux);
						}
					}
				}
			}
			System.out.println("---------------------------"); //eh para ser 2
			bwResult.write("---------------------------");
			bwResult.newLine();
			bwResult.close();
		}		
		
		return count;
	}
	
	public void addChildToHash(NodeTree nodeAux, ArrayList<EvolutionEnum> sequencePattern, HashMap<NodeTree, ArrayList<Double>> hashAux, 
			double d, ArrayDeque<NodeTree> queue){
		ArrayList<Double> arrayNodeAux = null;
				
		//caso o noh pai nao tenha sequencia de evolucao semelhante a buscada 
		for (NodeTree nodeAuxChild : nodeAux.getChildNodes()) {
			ArrayList<ArrayList<NodeTree>> arrayClusters = new ArrayList<ArrayList<NodeTree>>();

			arrayNodeAux = null;
			//para os nos filhos ainda nao visitados
			if(!nodeAuxChild.isVisited)	{
				nodeAuxChild.isVisited=true;
				queue.add(nodeAuxChild);
			}
			else {
				//pode nao satisfazer a sequencia, mas se satisfizer (por jah ter sido visitado) 
				//estah em hashAux
				arrayNodeAux = hashAux.get(nodeAuxChild);
			}
			//no filho tem evolucao semelhante a primeira evolucao buscada 
			if(nodeAuxChild.getEvolution()==sequencePattern.get((int) d)){
				//recebe a lista de caminhos que o cluster pai possui
				if(d!=0){
					for (ArrayList<NodeTree> arrayList : result.get(nodeAux)) {
							ArrayList<NodeTree> a = new ArrayList<NodeTree>();
							for (NodeTree nodeTree : arrayList) {
								a.add(nodeTree);
							}
							arrayClusters.add(a);
					}
				}
				

				if(arrayNodeAux==null){
					arrayNodeAux = new ArrayList<Double>();
				}						
				//adicionar a continuacao dos outros segmentos de clusters frequentes, nodeAuxChild
				if(!existsPosToCheck(arrayNodeAux,d)){
					arrayNodeAux.add(d);		
				}
				else System.out.println("O noh "+nodeAuxChild.getIdNode()+" descartou a posicao "+d);

				//para cada caminho do noh pai que nodeAuxChild deva ser inserido, ele eh adicionado
				for (int i = 0; i < arrayClusters.size(); i++) {

					//nesse caso, o caminho de tamanho igual a posToCheck deve receber NodeAuxChild
					NodeTree lastNode = arrayClusters.get(i).get(arrayClusters.get(i).size()-1);
					if(arrayClusters.get(i).size()==d && isChild(lastNode,nodeAuxChild)){
						arrayClusters.get(i).add(nodeAuxChild);
					}
				}
				
				//a partir desse no visitado pode haver uma nova sequencia semelhante a buscada
				if(d!=0 && nodeAuxChild.getEvolution()==sequencePattern.get(0)) {
					if(!existsPosToCheck(arrayNodeAux,0)) arrayNodeAux.add((double) 0);	
					else System.out.println("O noh "+nodeAuxChild.getIdNode()+" descartou a posicao "+d);
					
					//arrayNodeAux.add(0);					
					//nova sequencia que contem nodeAuxChild como cluster e que aparece com frequencia.
					//cria um novo caminho de clusters que contem nodeAuxChild como primeiro
					ArrayList<NodeTree> arrayNewCluster = new ArrayList<NodeTree>();
					arrayNewCluster.add(nodeAuxChild);
					arrayClusters.add(arrayNewCluster);					
				}
				if(d==0 && arrayClusters.isEmpty()){
					ArrayList<NodeTree> arrayNewCluster = new ArrayList<NodeTree>();
					arrayNewCluster.add(nodeAuxChild);
					arrayClusters.add(arrayNewCluster);
				}
				
				hashAux.put(nodeAuxChild, arrayNodeAux);
				if(result.containsKey(nodeAuxChild)){
					result.get(nodeAuxChild).addAll(arrayClusters);
				}
				else result.put(nodeAuxChild, arrayClusters);
				
			}
			else{
				if(d!=0 && nodeAuxChild.getEvolution()==sequencePattern.get(0)) {
					if(arrayNodeAux==null){
						arrayNodeAux = new ArrayList<Double>();
					}
					if(!existsPosToCheck(arrayNodeAux,0)) arrayNodeAux.add((double) 0);	
					else System.out.println("O noh "+nodeAuxChild.getIdNode()+" descartou a posicao "+d);
					//arrayNodeAux.add(0);

					//arrayClusters.addAll(result.get(nodeAux));
					
					//nova sequencia que contem nodeAuxChild como cluster e que aparece com frequencia.
					//cria um novo caminho de clusters que contem nodeAuxChild como primeiro
					ArrayList<NodeTree> arrayNewCluster = new ArrayList<NodeTree>();
					arrayNewCluster.add(nodeAuxChild);
					arrayClusters.add(arrayNewCluster);
					hashAux.put(nodeAuxChild, arrayNodeAux);
					if(result.containsKey(nodeAuxChild)){
						result.get(nodeAuxChild).addAll(arrayClusters);
					}
					else result.put(nodeAuxChild, arrayClusters);
				}
				
			}
		
		}//end for
	}
	private boolean existsPosToCheck(ArrayList<Double> arrayNodeAux,
			double d) {
		for (Double Double : arrayNodeAux) {
			if(Double==d) return true;
		}
		return false;
	}

	private boolean isChild(NodeTree lastNode, NodeTree nodeAuxChild) {
		ArrayList<NodeTree> childs = lastNode.getChildNodes(); 
		for (int i = 0; i < childs.size(); i++) {
			NodeTree childNode = childs.get(i);
			if(nodeAuxChild.getIdNode()==childNode.getIdNode() && nodeAuxChild.time==childNode.time
					&& nodeAuxChild.getEvolution().toString().contains(childNode.getEvolution().toString())) return true;
		}
		return false;
	}

	public void setNotVisited(){
		ArrayList<NodeTree> q = new ArrayList<NodeTree>();
		NodeTree n;
		q.add(root);
		while (q.size() > 0)
		 {
		    n = q.remove(0);
		    n.isVisited=false;
		    for (NodeTree nodeTree : n.getChildNodes()) {
				if(nodeTree.isVisited)q.add(nodeTree);
			}
		 }
	}
	
	public void closeBwResult() throws IOException{
		bwResult.close();
	}
	
	public static void main(String[] args) throws IOException {
		NodeTree root = new NodeTree(EvolutionEnum.ROOT,0,1);
		NodeTree child1 = new NodeTree(EvolutionEnum.MERGE, 1,2);
		NodeTree child2 = new NodeTree(EvolutionEnum.APPEARS,2,2);
		NodeTree child3 = new NodeTree(EvolutionEnum.APPEARS, 3,2);
		NodeTree child4 = new NodeTree(EvolutionEnum.MERGE, 4,3);
		NodeTree child5 = new NodeTree(EvolutionEnum.MERGE, 5,3);
		NodeTree child6 = new NodeTree(EvolutionEnum.MERGE, 6,3);
		NodeTree child7 = new NodeTree(EvolutionEnum.SPLIT, 7,3);
		NodeTree child8 = new NodeTree(EvolutionEnum.MERGE,8,4);
		NodeTree child9 = new NodeTree(EvolutionEnum.SPLIT,9,4);
		NodeTree child10 = new NodeTree(EvolutionEnum.MERGE,10,5);

		
		Tree tree = new Tree(root);
		
		root.addChildNode(child1);
		root.addChildNode(child2);
		root.addChildNode(child3);
		child1.addChildNode(child4);
		child1.addChildNode(child5);
		child2.addChildNode(child6);
		child3.addChildNode(child7);
		child5.addChildNode(child8);
		child5.addChildNode(child9);
		child9.addChildNode(child10);
		
		tree.printAllRootToLeafPaths(root, new ArrayList<NodeTree>());
		ArrayList<EvolutionEnum> sequence = new ArrayList<EvolutionEnum>();
		sequence.add(EvolutionEnum.MERGE);
		sequence.add(EvolutionEnum.MERGE);
		sequence.add(EvolutionEnum.SPLIT);
		sequence.add(EvolutionEnum.MERGE);
		System.out.println(tree.countSubgraphsIsomorphous(sequence,1));
		//Apriori.aprioriMethod(tree, 1, 4);
	}	
}

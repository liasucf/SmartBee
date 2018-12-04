package EuclideanDistance;

import java.io.BufferedReader;
import java.lang.Object;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;

import evolutionEnum.EvolutionEnum;
import tree.NodeTree;
import tree.Tree;

public class euclideanDistance {
	
	public static double [][] M;
	public static NodeTree root = new NodeTree(EvolutionEnum.valueOf("ROOT"), -1,0);		
	public static Tree tree = new Tree(root);

	public static double euclideanDistance(ArrayList<Double> cluster1, ArrayList<Double> cluster2){
		

		    double Sum = 0.0;

		    for (int i = 0; i < cluster1.size(); i++) {
		        Sum = Sum + Math.pow(cluster1.get(i) - cluster2.get(i) , 2.0);
		    }

		    return Math.sqrt(Sum);
		}
	
	
	//Pegando os valores dos arquivos txt e colocando p dentro dos arrays 
	//Chama a funcao jaccard e ver se o seu valor da diferente de zero
	public static void printMatrix(String clustersFirstTimestamp, String clustersSecondTimestamp, int countClustersAnt, int countClustersAct) throws IOException{
		M = new double[countClustersAnt][countClustersAct];
		BufferedReader br1, br2;
		br1 = new BufferedReader(new FileReader(clustersFirstTimestamp));
		
		String l1, l2;
		String[] line1, line2;
		ArrayList<Double> cluster1, cluster2;
		int i=0, j = 0;
				
		while((l1 = br1.readLine())!=null){
			l1 = l1.replace("<", "");
			l1 = l1.replace(">", "");
			l1 = l1.replaceAll(" ","");
			line1 = l1.split(",");
		
			cluster1 = new ArrayList<Double>();			
			for (String id : line1) {
				try {
				cluster1.add(new Double(id));}
				catch (NumberFormatException e){
			       System.out.println("not a number"); 
			   } 
			}
			
			br2 = new BufferedReader(new FileReader(clustersSecondTimestamp));
			while((l2 = br2.readLine())!=null){
				l2 = l2.replace("<", "");
				l2 = l2.replace(">", "");
				l2 = l2.replaceAll(" ", "");
				line2 = l2.split(",");

				cluster2 = new ArrayList<Double>();
				for (String id : line2) {
					try {
					cluster2.add(Double.parseDouble(id));}
				catch (NumberFormatException e){
				       System.out.println("not a number"); 
				   } 
				}
				double euclideanDistance = euclideanDistance(cluster1, cluster2);
				M[i][j] = euclideanDistance;
				if(euclideanDistance!=0){
					System.out.println("Cluster do primeiro timestamp "+i+": "+l1);
					System.out.println("Cluster do segundo timestamp "+j+": "+l2);
					System.out.println("M["+i+","+j+"]="+euclideanDistance);
					System.out.println("------");
				}
				j++;
			}
			br2.close();
			i++;							
		}
		br1.close();
	}
	
	
	
	//matriz M que armazena em cada célula o valor de similaridade entre dois clusters em diferentes timestamps. 
	//Nesta matriz, as linhas representam os clusters que pertencem a Ct e as colunas representam os clusters que pertencem a Ct + δt
	public static void discoverEvolution(int countClustersAnt, int countClustersAct, String timeDuration, ArrayList<Double> suportClusterAnt,ArrayList<Double> suportClusterAct, int time){
		//variaveis de auxilio
		NodeTree nodeAux, nodeAux2, newNode;
		HashMap<Double,NodeTree> newHash = new HashMap<Double,NodeTree>();
		HashMap<Double, NodeTree> hashAux = tree.getHashEvolution();
		
		for (int i = 0; i < countClustersAnt; i++) {
			String split="";
			int countSplit=0;
			boolean disappear=true;
			for (int j = 0; j < countClustersAct; j++) {
			
					//[idCluster, TimeInterval, EvolutionAction, Support]. 
					//Um cluster sobreviveu, continuou da mesma forma entre um intervalo de tempo e outro, e persistiu como um novo cluster
					//cluster survives
					
					//it is (usually) 1 if they are the same, and
					//it is in the range [0, 1].

					if(M[i][j]>0.8) {
						disappear=false;
						System.out.println("Cluster:"+i+",Time: "+timeDuration+",Evolution: Survives to Cluster"+j+",Suport: "+suportClusterAnt.get(i));
						nodeAux = hashAux.get(i);
						newNode = new NodeTree(EvolutionEnum.SURVIVES, j, time);
						//nodeAux.addChildNode(newNode);
						newHash.put((double) j, newNode);
					}
				
				
			}
			
			//ou ocorreu um merge ou expansao ou retracao
			if(countSplit==1){
				double clusterj = Double.parseDouble(split.replace(" ", ""));
				split="";
				
					//ocorre uma expansao
					if(suportClusterAct.get((int) clusterj)>suportClusterAnt.get(i)){
						System.out.println("Cluster:"+i+",Time: "+timeDuration+",Evolution: Expands to "+clusterj+",Suport: "+suportClusterAnt.get(i));
						//from
						nodeAux = hashAux.get(i);
						//to
						newNode = new NodeTree(EvolutionEnum.EXPANDS, clusterj, time);
						nodeAux.addChildNode(newNode);
						if(newHash.containsKey(clusterj)) System.out.println("TEM PERIGO!!!!");
						newHash.put((double) clusterj, newNode);
					}
					//ocorre uma retracao
					else {
						System.out.println("Cluster:"+i+",Time: "+timeDuration+",Evolution: Shrinks to "+clusterj+",Suport: "+suportClusterAnt.get(i));
						nodeAux = hashAux.get(i);	
						newNode = new NodeTree(EvolutionEnum.SHRINKS, clusterj, time);
						nodeAux.addChildNode(newNode);
						if(newHash.containsKey(clusterj)) System.out.println("TEM PERIGO!!!!");
						newHash.put((double) clusterj, newNode);
					}

				}
		
			else if(disappear){
				System.out.println("Cluster:"+i+",Time: "+timeDuration+",Evolution: Disappears,Suport: "+suportClusterAnt.get(i));
				nodeAux = hashAux.get(i);	
				newNode = new NodeTree(EvolutionEnum.DISAPPEARS, -1, time);
				//nodeAux.addChildNode(newNode);
			}
		}
		System.out.println("Aparecimento de Novos Clusters");
		for (int j = 0; j < countClustersAct; j++) {
			boolean appear=true;
			for (int i = 0; i < countClustersAnt && appear; i++) {
				if(M[i][j]>0.8) appear = false;
			}
			if(appear) {
				System.out.println("Cluster:"+j+",Time: "+timeDuration+",Evolution: Appears,Suport: "+suportClusterAct.get(j));	
				newNode = new NodeTree(EvolutionEnum.APPEARS ,j, time);
				root.addChildNode(newNode);
				newHash.put((double) j, newNode);
			}
		}
		hashAux.clear();
		tree.setHashEvolution(newHash);		
	}
	public static void initTreeEvolution(ArrayList<Double> suportClusterAct, int time){
		NodeTree newNode;
				
		for (int i = 0; i < suportClusterAct.size(); i++) {
			newNode = new NodeTree(EvolutionEnum.APPEARS,i, time);
			root.addChildNode(newNode);
			tree.getHashEvolution().put((double) i, newNode);
		}
	}



	public static void main(String[] args) {
		try {
			
//			int i=12;
//			while(i <17) {
//			System.out.println("Clusters do primeiro timestamp e segundo");
//			euclideanDistance.printMatrix("C:\\Users\\Lia\\Desktop\\Codigos- Evolução de Clusters\\temp"+i+".txt", 
//					"C:\\Users\\Lia\\Desktop\\Codigos- Evolução de Clusters\\temp"+(i+1)+".txt", 1, 1);
//			
//					
//			
//			BufferedReader br1, br2;
//			br1 = new BufferedReader(new FileReader("C:\\Users\\Lia\\Desktop\\Codigos- Evolução de Clusters\\temp"+i+".txt"));
//			br2 = new BufferedReader(new FileReader("C:\\Users\\Lia\\Desktop\\Codigos- Evolução de Clusters\\temp"+(i+1)+".txt"));
//			String l1, l2;
//			String[] line1, line2;
//			ArrayList<Double> cluster1, cluster2;
//			
			System.out.println("Clusters do primeiro timestamp e segundo");
			euclideanDistance.printMatrix("C:\\Users\\Lia\\Desktop\\Codigos- Evolução de Clusters\\temp1.txt", 
					"C:\\Users\\Lia\\Desktop\\Codigos- Evolução de Clusters\\temp2.txt", 1, 4);			
					
			
			BufferedReader br1, br2;
			br1 = new BufferedReader(new FileReader("C:\\Users\\Lia\\Desktop\\Codigos- Evolução de Clusters\\temp1.txt"));
			br2 = new BufferedReader(new FileReader("C:\\Users\\Lia\\Desktop\\Codigos- Evolução de Clusters\\temp2.txt"));
			String l1, l2;
			String[] line1, line2;
			ArrayList<Double> cluster1, cluster2;
					
			
			while((l1 = br1.readLine())!=null){
				l1 = l1.replace("<", "");
				l1 = l1.replace(">", "");
				l1 = l1.replaceAll(" ","");
				line1 = l1.split(",");
			
				cluster1 = new ArrayList<Double>();			
				for (String id : line1) {
					try {
					cluster1.add(new Double(id));}
				catch (NumberFormatException e){
				       System.out.println("not a number"); 
				   } 
				}
				
				
				while((l2 = br2.readLine())!=null){
					l2 = l2.replace("<", "");
					l2 = l2.replace(">", "");
					l2 = l2.replaceAll(" ", "");
					line2 = l2.split(",");

					cluster2 = new ArrayList<Double>();
					
					for (String id : line2) {
						try {
							cluster2.add(Double.parseDouble(id));}
						catch (NumberFormatException e){
							System.out.println("not a number"); 
					   } 
					}
				
				euclideanDistance.discoverEvolution(3, 6, "10",cluster1,cluster2, 5);
					
					
					
		}}
			br1.close();
			br2.close();
//			i++;
//			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}

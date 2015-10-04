import akka.actor._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.concurrent._
import scala.concurrent.duration._
import scala.math._
//import scala.concurrent.ExecutionContext.Implicits.global

/*
*manager case classes
*/
case class CreateCommunicationNetwork(numberOfNodes:Int, topology:String, algorithm: String)
case class Start(algorithm:String)
case class ImTransmitting(name:String)

/*
*worker case classes
*/
case class SetNeighbors(neighborsFromMaster:ArrayBuffer[ActorRef])
case class PreparePushSum(sum:Double, weight:Double)
case class RemoveNeighbor(neighbor:ActorRef)
case class PushSum(sum:Double, weight:Double)
case class ForwardPushSum(sum:Double, weight:Double)
case class FinishPushSum(ratio:Double, name:String)
case object Rumor //for gossip communication
case object Gossip //for transmitting rumor
case class Finish(name:String) //Node tells that its done. 
case class AssignRandomNeighbor(random:ActorRef)



object Main extends App{
	override def main(args: Array[String])
	{
		try{
			var numberOfNodes:Int = args(0).toInt
			var topology:String = args(1)
			var algorithm:String = args(2)

			//wake up the manager and start it to create the network 
			val system = ActorSystem("NetworkManager")
			//val manager = system.actorOf(Props(new Manager(numberOfNodes,topology,algorithm)), name = "manager")
			val manager = system.actorOf(Props[Manager], name = "manager")
			manager ! CreateCommunicationNetwork(numberOfNodes, topology, algorithm)
		}
		catch{
			case e:Exception => println("Enter the Right Data, Make sure to add arguments: "+e.toString())
		}		
	}
	
}

class Manager extends Actor{
	var nodeList = new ArrayBuffer[ActorRef]()
	var time:Long = 0
	var numNodesLeft:Int = 0	
	var numNodesTransmitting:Int = 0;
	var numNodesDone:Int = 0;
	var totalNodes:Int = 0;
	println("I've been Summoned Fellas")
	def receive = {
		case CreateCommunicationNetwork(numberOfNodes, topology, algorithm) => {
			if(topology == "3D" || topology == "Imp3D"){
				totalNodes = pow((pow(numberOfNodes, 1.0/3.0) + .001).toInt,3.0).toInt
				println("Spreading " + algorithm + " to " + totalNodes + " nodes")
				numNodesLeft = totalNodes
			}
			else{
				totalNodes = numberOfNodes
				numNodesLeft = numberOfNodes
			}
			for(i <- 0 until totalNodes){
				nodeList += context.actorOf(Props[GossipPushSumSimulator], name = "node"+i)
				/*
				* set the sum and weight at each node for if the algorithms is pushSum
				* it won't be used in gossip network. 
				*/
				nodeList(i) ! PreparePushSum(i, 1) //weight is going to be one for the calculating average. 
			}
			topology match {
				case "full" => {
					for(i <- 0 until numberOfNodes) {
						nodeList(i)!SetNeighbors(nodeList - nodeList(i))
					}
				}

				case "line" => {
					var tempBuffer = new ArrayBuffer[ActorRef]()
					//set the first and last nodes neighbor (only one)
					nodeList(0)!SetNeighbors(tempBuffer+=nodeList(1))
					tempBuffer=new ArrayBuffer[ActorRef]()//tempBuffer.clear()
					nodeList(numberOfNodes-1)!SetNeighbors(tempBuffer+=nodeList(numberOfNodes - 2))

					//set all other neighbors. 
					for(i <- 1 to numberOfNodes - 2){
						tempBuffer=new ArrayBuffer[ActorRef]()
						tempBuffer += (nodeList(i-1), nodeList(i+1))
						nodeList(i)!SetNeighbors(tempBuffer)
					}
				}
				case "3D" => {
					val cuberoot = (pow(numberOfNodes, 1.0/3.0) + .001).toInt 
					val gridSize = pow(cuberoot,3).toInt
					val nodes_plane = pow(cuberoot,2).toInt

					for(i<-0 to gridSize-1)
					{
						var tempBuffer = new ArrayBuffer[ActorRef]()
						var j = 123;

						j = i/nodes_plane;

						if(i-1>=0)
						{
							if( (i-1)/nodes_plane == j && (i-1) / cuberoot == i/cuberoot)
							{
								tempBuffer += nodeList(i-1);
								//nodeList(i)!SetNeighbors(nodeList(i-1))
							}
						}
						
						if((i+1) < ((j+1)*nodes_plane))
						{
							if( (i+1)/nodes_plane == j && (i+1) / cuberoot == i/cuberoot )
							{
								tempBuffer += nodeList(i+1);
								//nodeList(i)!SetNeighbors(nodeList(i+1))
							}

						}

						if((i-cuberoot) >= 0)
						{
							if( ((i-cuberoot)/nodes_plane).toInt == j )
							{
								tempBuffer += nodeList(i-cuberoot);
								//nodeList(i)!SetNeighbors(nodeList(i-cuberoot))
							}
						}

						if((i+cuberoot) < ((j+1)*nodes_plane))
						{
							if( ((i+cuberoot)/nodes_plane).toInt == j )
							{
								tempBuffer += nodeList(i+cuberoot);
								//nodeList(i)!SetNeighbors(nodeList(i+cuberoot))
							}					
						}

						if(i-nodes_plane >= 0)
						{
							if( ((i-nodes_plane)/nodes_plane).toInt == j-1 )
							{
								tempBuffer += nodeList(i-nodes_plane);
								//nodeList(i)!SetNeighbors(nodeList(i-nodes_plane))
							}	
						}

						if(i+nodes_plane < gridSize)
						{
							if( ((i+nodes_plane)/nodes_plane).toInt == j+1)
							{
								tempBuffer += nodeList(i+nodes_plane);
								//nodeList(i)!SetNeighbors(nodeList(i+nodes_plane))
							}	
						}
						nodeList(i)!SetNeighbors(tempBuffer);
					}
				}  
				
				case "Imp3D" =>  {
					//to be implemented

					val cuberoot = (pow(numberOfNodes, 1.0/3.0) + .001).toInt ;
					val gridSize = pow(cuberoot,3).toInt;
					val nodes_plane = pow(cuberoot,2).toInt;
					var countImp3D:Int = 0;
					for(i<-0 to gridSize-1)
					{
						var tempBuffer = new ArrayBuffer[ActorRef]()
						var indexMonitor = new ArrayBuffer[Int]()
						var j = 123;
						countImp3D += 1
						j = i/nodes_plane;

						if(i-1>=0)
						{
							if( (i-1)/nodes_plane == j && (i-1) / cuberoot == i/cuberoot)
							{
								tempBuffer += nodeList(i-1)
								indexMonitor += i-1
								//nodeList(i)!SetNeighbors(nodeList(i-1))
							}
						}
						
						if((i+1) < ((j+1)*nodes_plane))
						{
							if( (i+1)/nodes_plane == j && (i+1) / cuberoot == i/cuberoot )
							{
								tempBuffer += nodeList(i+1)
								indexMonitor += i+1
								//nodeList(i)!SetNeighbors(nodeList(i+1))
							}

						}

						if((i-cuberoot) >= 0)
						{
							if( ((i-cuberoot)/nodes_plane).toInt == j )
							{
								tempBuffer += nodeList(i-cuberoot)
								indexMonitor += i-cuberoot
								//nodeList(i)!SetNeighbors(nodeList(i-cuberoot))
							}
						}

						if((i+cuberoot) < ((j+1)*nodes_plane))
						{
							if( ((i+cuberoot)/nodes_plane).toInt == j )
							{
								tempBuffer += nodeList(i+cuberoot)
								indexMonitor += i+cuberoot
								//nodeList(i)!SetNeighbors(nodeList(i+cuberoot))
							}					
						}

						if(i-nodes_plane >= 0)
						{
							if( ((i-nodes_plane)/nodes_plane).toInt == j-1 )
							{
								tempBuffer += nodeList(i-nodes_plane)
								indexMonitor += i-nodes_plane
								//nodeList(i)!SetNeighbors(nodeList(i-nodes_plane))
							}	
						}

						if(i+nodes_plane < gridSize)
						{
							if( ((i+nodes_plane)/nodes_plane).toInt == j+1)
							{
								tempBuffer += nodeList(i+nodes_plane)
								indexMonitor += i+nodes_plane
								//nodeList(i)!SetNeighbors(nodeList(i+nodes_plane))
							}	
						}

						
						//val r = scala.util.Random;
						/*if(algorithm == "gossip"){
							if(countImp3D % (gridSize/2) == 0){
								var random_num_new_neighbor = Random.nextInt(gridSize)
								if(indexMonitor.exists(_ == random_num_new_neighbor) == true|| random_num_new_neighbor == i){
									while (indexMonitor.exists(_ == random_num_new_neighbor) == true || random_num_new_neighbor == i){
										random_num_new_neighbor = Random.nextInt(gridSize - 1)
									}
									tempBuffer += nodeList(random_num_new_neighbor)
								}	
							}
						}
						else{
							var random_num_new_neighbor = Random.nextInt(gridSize)
								if(indexMonitor.exists(_ == random_num_new_neighbor) == true|| random_num_new_neighbor == i){
									while (indexMonitor.exists(_ == random_num_new_neighbor) == true || random_num_new_neighbor == i){
										random_num_new_neighbor = Random.nextInt(gridSize - 1)
									}
									tempBuffer += nodeList(random_num_new_neighbor)
								}	
						}*/

						var random_num_new_neighbor = Random.nextInt(gridSize)
						if(indexMonitor.exists(_ == random_num_new_neighbor) == true|| random_num_new_neighbor == i){
							while (indexMonitor.exists(_ == random_num_new_neighbor) == true || random_num_new_neighbor == i){
								random_num_new_neighbor = Random.nextInt(gridSize - 1)
							}
							tempBuffer += nodeList(random_num_new_neighbor)
							nodeList(random_num_new_neighbor) ! AssignRandomNeighbor(nodeList(i))
						}	


						/*println("Neighbors of" + i  + ": ")
						for(k<-0 to tempBuffer.length-1)
						{
							println(tempBuffer(k))
						}*/
						

						nodeList(i)!SetNeighbors(tempBuffer);
					}
				} 

				case _ => {
					println("Unrecognized Communication network")
					context.system.shutdown()
				}
			}
			self ! Start(algorithm)
		}
		case Start(algorithm) => {
			//println("Implement " + algorithm)
			time = System.currentTimeMillis()
			algorithm match {
				case "gossip" => {
					var randomNode = Random.nextInt(nodeList.length)
					//start a rumor on a random node
					nodeList(randomNode) ! Rumor
				} 
				case "push-sum" => {
					var randomNode = Random.nextInt(nodeList.length)
					nodeList(randomNode) ! PushSum(0.0,0.0)
				}
				case _ => {
					println("Unrecognized Algorithm")	
					context.system.shutdown()
				} 
			}	
		}
		case ImTransmitting(name) => {
			//println(name + " is transmitting")
			numNodesTransmitting += 1
			numNodesDone += 1
		}
		case Finish(name) => {
			
			numNodesLeft -= 1
			numNodesDone -= 1
			//println(name+ " done: " + numNodesDone)
			if(numNodesLeft == 0){
				println("Number of Nodes Transmitted: " + numNodesTransmitting)
				println("Number of Nodes Left: " + numNodesDone)
				println("Time to Spread Gossip: " + (System.currentTimeMillis() - time) + " miliseconds")
				context.system.shutdown()
			}
			else if(numNodesDone == 0){
				println("Number of Nodes Transmitted: " + numNodesTransmitting)
				println("Number of Nodes didn't receive the message: " + numNodesLeft)
				context.system.shutdown()
			}
		} 
		case FinishPushSum (ratio, name) => {
			numNodesLeft -= 1
			numNodesDone -= 1
			println ("PushSum from " + name + ": " + ratio )
			println("Time to compute: " + (System.currentTimeMillis() - time) + " miliseconds")
			context.system.shutdown()
			/*if(numNodesLeft == 0){
				println("Number of Nodes Transmitted: " + numNodesTransmitting)
				println("Number of Nodes Left: " + numNodesDone)
				println("Time to Spread PushSum: " + (System.currentTimeMillis() - time) + " miliseconds")
				context.system.shutdown()
			}
			else if(numNodesDone == 0){
				println("Number of Nodes Transmitted: " + numNodesTransmitting)
				println("Number of Nodes didn't receive the message: " + numNodesLeft)
				context.system.shutdown()
			}*/
		}
	}
}


class GossipPushSumSimulator extends Actor{
	import context._
	var neighbors = new ArrayBuffer[ActorRef]()
	var rumorCount:Int = 0
	var manager:ActorRef = null
	var stepTime: Int = 10
	var nodeSum:Double = 0.0
	var nodeWeight:Double = 0.0
	var lastRatio: Double = 0.0
	var stabilityCount: Int = 0
	def receive = {
		case SetNeighbors(neighborsFromMaster) => {
			neighbors ++= neighborsFromMaster
			manager = sender
			//("self: "+self + " neighbors: " + neighbors)
			//context.system.shutdown()
		}
		case AssignRandomNeighbor(random) => {
			neighbors += random
		}
		case Rumor => {
			rumorCount += 1

			//println(self.path.name.toString() + ": " + rumorCount)
			if(rumorCount == 1){
				/*
				*to transmit it the first time. 
				*the transmit calls will be asynchronous
				*from this point. 
				*/
				self ! Gossip
				manager ! ImTransmitting(self.path.name.toString())
			}
			
			//self ! TransmitRumor()
		}
		case Gossip => {
			//stop transmitting if rumor count is 10.
			if(rumorCount >= 10 || neighbors.length <= 0){
				//delete itself from all its neghbors.
				for( i <- 0 until neighbors.length) {
				 	neighbors(i) ! RemoveNeighbor(self)
				 } 
				 /*
				 *tell manager that i am done. and stop
				 */
				 manager ! Finish(self.path.name)
				 context.stop(self)
			}
			else{
				//transmit the rumor to a random node
				var randomNode = Random.nextInt(neighbors.length)
				neighbors(randomNode) ! Rumor
				context.system.scheduler.scheduleOnce(stepTime milliseconds, self, Gossip)
			}
		}
		case RemoveNeighbor(neighbor) => {
			neighbors = neighbors - neighbor
			/*if(neighbors.length == 0){
				println(self.path.name.toString() + " neighbors are zero")
				manager ! Finish
				for( i <- 0 until neighbors.length) {
				 	neighbors(i) ! RemoveNeighbor(self)
				 } 
				 context.stop(self) 
			}*/
		}

		/*methods specific to PushSum*/

		case PreparePushSum(sum,weight) => {
			nodeSum = sum
			nodeWeight = weight
		} 

		case PushSum(sum,weight) => {

			nodeSum += sum
			nodeWeight += weight

			//send the push sum
			nodeSum/=2
			nodeWeight/=2

			self ! ForwardPushSum(nodeSum, nodeWeight)	
		}

		case ForwardPushSum(sum,weight) => {
			var newRatio = nodeSum/nodeWeight
			if(Math.abs(lastRatio - newRatio) <= 0.0000000001){
				stabilityCount+=1
			}
			else{
				stabilityCount = 0
			}
			lastRatio = newRatio
			if(stabilityCount < 3 && neighbors.length >0){
				var randomNode = Random.nextInt(neighbors.length)
				neighbors(randomNode) ! PushSum(sum,nodeWeight)
				//println(self.path.name.toString() + ": " + lastRatio);
			}
			else{
				for( i <- 0 until neighbors.length) {
				 	neighbors(i) ! RemoveNeighbor(self)
				 } 
				 /*
				 *tell manager that i am done. and stop
				 */
				 manager ! FinishPushSum(nodeSum/nodeWeight, self.path.name.toString())
				 context.stop(self)
			}
		}
		
	}
}


package Thesis.Thesisbuild

import stapl.core._

class TreeConverter(val root: AbstractPolicy, val knownAttributes : List[Attribute]) {
  
  /****************************************************************************
   ****************************************************************************
   ****************************MAIN  ALGORITHM*********************************
   ****************************************************************************
   ****************************************************************************/
  
	def reduce(policy : Policy) : AbstractPolicy = {
	  var policyList = List[Policy]()
	  var reduceList:List[Policy] = Nil
	  while(hasPolicyChildren(policy)) {
	   policyList = getLowestPolicies(policy)
	   reduceList = List[Policy]()
	   for(p <- policyList){
	     reduceList ::= reduceLeaves(p)
	   }
	   for(p <- reduceList){
	     var parent= p.parent.asInstanceOf[Policy]
	     convertCA(p,parent.pca)
	     combinePolicies(p,parent)
	   }
	  }
	 
	  root
	}
	
	def normalise(policy : Policy) : AbstractPolicy = {
	  //TODO: copy policy?
	  var p = null
	  for (p <- policy.subpolicies){
	    //TODO: normalise condition subpolicies
	  }
	  //TODO: return normalised policy
	  root
	}
	
	def expand() : AbstractPolicy = {
	  //TODO: expand normalised tree
	  root
	}
	
	/***************************************************************************
	****************************************************************************
	****************************HELPER  FUNCTIONS*******************************
	****************************************************************************
	****************************************************************************/
	
	def hasPolicyChildren(policy:Policy) = policy.subpolicies.exists(_.isInstanceOf[Policy]) 
	
	def getLowestPolicies(policy:Policy) : List[Policy] = {
	  var resultList = List[Policy]()
	  var children = policy.subpolicies
	  var includethis = true
	  for(c <- children)
	  {
	    if(c.isInstanceOf[Policy]){
	      includethis = false
	      resultList :::= getLowestPolicies(c.asInstanceOf[Policy])
	    }
	  }
	  if(includethis) return policy :: resultList else return resultList
	}
	
	def reduceLeaves(policy : Policy) : Policy = {
	  var ca = policy.pca
	  var newPolicy:Policy = null
	  if(ca == DenyOverrides || ca == PermitOverrides){
	    var rule1 = combineEffect(policy,Permit)
	    var rule2 = combineEffect(policy,Deny)
	    var subpolicies: List[AbstractPolicy] = List(rule1,rule2)
	    //TODO: rekening houden met nullpointers uit combineEffect
	    newPolicy = new Policy(policy.id)(policy.target,ca,subpolicies,policy.obligations)
	  }else {
		newPolicy = createFAChain(policy)
	  }
	  return newPolicy
	}
	
	def combineEffect(policy: Policy, effect:Effect) : Rule = {
	  var subpols = policy.subpolicies
	  var children = subpols.filter(c => (c.asInstanceOf[Rule]).effect == effect)
	  var conditions = List[Expression]()
	  var newObs = List[ObligationAction]()
	  var newRule:Rule = null
	  if(children.length > 0){
	    for(c <- children){
	    	conditions ::= c.asInstanceOf[Rule].condition
	    	newObs :::= c.asInstanceOf[Rule].obligationActions
	    }
	    var newExpression = conditions(0)
	    for(i <- 1 to conditions.length-1){
	      newExpression = newExpression | conditions(i)
	    }
	    newRule = new Rule(children(0).id)(effect,newExpression,newObs)
	  }
	  return newRule
	}
	
	def createFAChain(policy: Policy) : Policy = {
	  var parent = policy.parent
	  var newPol = FAChain(policy.subpolicies.map(x => x.asInstanceOf[Rule]), 0 , policy.id)
	  var children = parent.asInstanceOf[Policy].subpolicies
	  children = replace(children, policy,newPol)
	  //TODO fix parent reference
	  return getLowestPolicies(newPol).head
	}
	
	def replace(policies : List[AbstractPolicy], policy:Policy, newPolicy : Policy) : List[AbstractPolicy] = {
	  var resList = List[AbstractPolicy]()
	  for(x <- policies){
	    if(x == policy) resList ::= newPolicy else resList ::= x
	  }
	  return resList
	}
	
	def FAChain(rules : List[Rule], id : Int, startstring : String) : Policy = {
	    var Ca:CombinationAlgorithm = DenyOverrides
		if(rules(0).effect == Permit) {
		  Ca = PermitOverrides
		}
		var children = List[AbstractPolicy]()
		var leftChild = rules(0)
		var rightChild:AbstractPolicy = rules(1)
		if(rules.length > 2) 
		  rightChild = FAChain(rules.tail,id+1,startstring) 
		children = List(leftChild,rightChild)
		//TODO fix policy obligations
		return new Policy(startstring + id.toString)(true,Ca,children,null)
	}
	
	def convertCA(policy: Policy, ca: CombinationAlgorithm) : Policy = {
	  //TODO implement
	  return policy
	}
	
	def combinePolicies(child : Policy, parent : Policy) : Policy = {
	  //TODO implement
	  return parent
	}

}
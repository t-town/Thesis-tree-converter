package Thesis.Thesisbuild

import stapl.core._
import aima.core.logic.propositional.parsing.ast.Sentence
import aima.core.logic.propositional.visitors.ConvertToDNF
import aima.core.logic.propositional.parsing.ast.ComplexSentence
import aima.core.logic.propositional.parsing.ast.Connective
import aima.core.logic.propositional.parsing.ast.AtomicSentence
import aima.core.logic.propositional.parsing.ast.PropositionSymbol

class TreeConverter(val root: AbstractPolicy, val knownAttributes : Set[Attribute]) {
  
  /****************************************************************************
   ****************************************************************************
   ****************************MAIN  ALGORITHM*********************************
   ****************************************************************************
   ****************************************************************************/
  
	def reduce(policy : Policy) : Policy = {
	  var policyList = List[Policy]()
	  var reduceList:List[Policy] = Nil
	  while(hasPolicyChildren(policy)) {
	   policyList = getLowestPolicies(policy)
	   reduceList = List[Policy]()
	   for(p <- policyList){
	     reduceList ::= reduceLeaves(p)
	   }
	   for(p <- reduceList){
	     var parent= p.parent.getOrElse(null).asInstanceOf[Policy]
	     var conversion = convertCA(p,parent.pca)
	     combinePolicies(conversion,parent)
	   }
	  }
	  var result = policy
	  if(policy.subpolicies.length>2)
	    if(policy.pca == FirstApplicable)
	       result = reduce(getHighestParent(reduceLeaves(policy)))
	    else
	    	result = reduceLeaves(policy)
	 return result
	}
	
	def normalise(policy : Policy) : AbstractPolicy = {
	  var p = null
	  var newsubs = List[AbstractPolicy]()
	  for (p <- policy.subpolicies.reverse){
	    var rule = p.asInstanceOf[Rule]
	    var sentence = convertToSentence(rule.condition)
	    sentence = ConvertToDNF.convert(sentence)
	    var condition = revertToCondition(sentence)
	    val newRule = new Rule(rule.id)(rule.effect,condition,List[ObligationAction]())
	    newRule.parent = Some(policy)
	    newsubs = newRule :: newsubs
	  }
	  policy.subpolicies = newsubs
	  index = 0
	  propMap = Map[String,Expression]()
	  return policy
	}
	
	def expand(policy : Policy) : AbstractPolicy = {
	  var splitPol = policy
	  var knownAtts = knownAttributes
	  while(canBeSplit(splitPol)){
	    var tmp = splitPolicy(splitPol,knownAtts)
	    tmp = splitRules(splitPol,knownAtts)
	    knownAtts ++= getAttributes(tmp.subpolicies.head.asInstanceOf[Policy])
	    splitPol = tmp
	  }
	  return policy
	}
	
	/***************************************************************************
	****************************************************************************
	***********************HELPER  FUNCTIONS REDUCTION**************************
	****************************************************************************
	****************************************************************************/
	
	def getHighestParent(policy:Policy): Policy = {
	  var continueLoop = true
	  var result = policy
	  while(continueLoop){
		result.parent match {
			case Some(x) => result = x
			case None => continueLoop = false
		}
	  }
	  return result
	}
	
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
	  if(includethis) 
	    return policy :: resultList 
	  else return resultList
	}
	
	def reduceLeaves(policy : Policy) : Policy = {
	  var ca = policy.pca
	  var newPolicy:Policy = null
	  if(policy.subpolicies.length <= 2)
	    return policy
	  if(ca == DenyOverrides || ca == PermitOverrides){
	    var rule1 = combineEffect(policy,Permit)
	    var rule2 = combineEffect(policy,Deny)
	    var subpolicies: List[AbstractPolicy] = List(rule1,rule2)
	    subpolicies = subpolicies.filter(c => c != null)
	    newPolicy = new Policy(policy.id)(policy.target,ca,subpolicies,policy.obligations)
	    newPolicy.parent = policy.parent
	    newPolicy.parent match {
	    	case Some(x) => x.subpolicies = replace(x.subpolicies,policy,newPolicy)
	    	case None => 
	    }
	  }else {
		newPolicy = createFAChain(policy)
	  }
	  return newPolicy
	}
	
	def combineEffect(policy: Policy, effect:Effect) : Rule = {
	  var subpols = policy.subpolicies
	  var children = subpols.filter(c => (c.asInstanceOf[Rule]).effect == effect)
	  var conditions = List[Expression]()
	  var newRule:Rule = null
	  if(children.length > 0){
	    for(c <- children.reverse){
	    	conditions ::= c.asInstanceOf[Rule].condition
	    }
	    var newExpression = conditions(0)
	    for(i <- 1 to conditions.length-1){
	      newExpression = newExpression | conditions(i)
	    }
	    newRule = new Rule(children(0).id)(effect,newExpression,List[ObligationAction]())
	  }
	  return newRule
	}
	
	def createFAChain(policy: Policy) : Policy = {
	  var parent = policy.parent
	  var newPol = FAChain(policy.subpolicies.map(x => x.asInstanceOf[Rule]), 0 , policy.id,policy.target,true)
	  newPol.parent = policy.parent
	  newPol.parent match {
	    case Some(x) => x.subpolicies = replace(x.subpolicies,policy,newPol)
	    case None => 
	  }
	  return getLowestPolicies(newPol).head
	}
	
	def replace(policies : List[AbstractPolicy], policy:Policy, newPolicy : Policy) : List[AbstractPolicy] = {
	  var resList = List[AbstractPolicy]()
	  for(x <- policies.reverse){
	    if(x == policy) resList ::= newPolicy else resList ::= x
	  }
	  return resList
	}
	
	def FAChain(rules : List[Rule], id : Int, startstring : String, target: Expression, includeTarget : Boolean) : Policy = {
	    var Ca:CombinationAlgorithm = DenyOverrides
		if(rules(0).effect == Permit) {
		  Ca = PermitOverrides
		}
		var children = List[AbstractPolicy]()
		var leftChild = rules(0)
		var rightChild:AbstractPolicy = rules(1)
		if(rules.length > 2) 
		  rightChild = FAChain(rules.tail,id+1,startstring, target, false) 
		children = List(leftChild,rightChild)
		if(includeTarget)
		  return new Policy(startstring + id.toString)(target,Ca,children,List[Obligation]())
		else
		  return new Policy(startstring + id.toString)(true,Ca,children,List[Obligation]())
	}
	
	def convertCA(policy: Policy, ca: CombinationAlgorithm) : Policy = {
	  if(policy.pca == ca)
	   return policy
	   var newpolicy: Policy = null
	   if(ca == PermitOverrides && policy.pca == DenyOverrides){
	     if(policy.subpolicies.length == 1){
	       newpolicy = new Policy(policy.id)(policy.target,PermitOverrides,policy.subpolicies)
	     }else if(policy.subpolicies(0).asInstanceOf[Rule].effect == Permit){
	       var A:Rule = policy.subpolicies(0).asInstanceOf[Rule]
	       var B:Rule = policy.subpolicies(1).asInstanceOf[Rule]
	       var newrule = new Rule(A.id)(Permit,And(A.condition,Not(B.condition)),List[ObligationAction]())
	       var subpolicies:List[AbstractPolicy] = List(B, newrule)
	       newpolicy = new Policy(policy.id)(policy.target,PermitOverrides,subpolicies)
	     }else{
	       var A:Rule = policy.subpolicies(0).asInstanceOf[Rule]
	       var B:Rule = policy.subpolicies(1).asInstanceOf[Rule]
	       var newrule = new Rule(B.id)(Permit,And(Not(A.condition),B.condition),List[ObligationAction]())
	       var subpolicies:List[AbstractPolicy] = List(A, newrule)
	       newpolicy = new Policy(policy.id)(policy.target,PermitOverrides,subpolicies)
	     }
	   }else if(ca == PermitOverrides && policy.pca == FirstApplicable){
	     if(policy.subpolicies.length == 1 || policy.subpolicies(0).asInstanceOf[Rule].effect == Permit ){
	       newpolicy = new Policy(policy.id)(policy.target, PermitOverrides,policy.subpolicies)
	     }else{
	       newpolicy = convertCA(new Policy(policy.id)(policy.target,DenyOverrides,policy.subpolicies)
	           ,PermitOverrides)
	     }
	     
	   }else if(ca == DenyOverrides && policy.pca == FirstApplicable){
	      if(policy.subpolicies.length == 1 || policy.subpolicies(0).asInstanceOf[Rule].effect == Deny ){
	       newpolicy = new Policy(policy.id)(policy.target, DenyOverrides,policy.subpolicies)
	     }else{
	       newpolicy = convertCA(new Policy(policy.id)(policy.target,PermitOverrides,policy.subpolicies)
	           ,DenyOverrides)
	     }
	   }else if(ca == DenyOverrides && policy.pca == PermitOverrides){
	     if(policy.subpolicies.length == 1){
	       newpolicy = new Policy(policy.id)(policy.target,PermitOverrides,policy.subpolicies)
	     }else if(policy.subpolicies(0).asInstanceOf[Rule].effect == Permit){
	       var A:Rule = policy.subpolicies(0).asInstanceOf[Rule]
	       var B:Rule = policy.subpolicies(1).asInstanceOf[Rule]
	       var newrule = new Rule(B.id)(Deny,(!A.condition) & B.condition,List[ObligationAction]())	
	       var subpolicies:List[AbstractPolicy] = List(A, newrule)
	       newpolicy = new Policy(policy.id)(policy.target,PermitOverrides,subpolicies)
	     }else{
	       var A:Rule = policy.subpolicies(0).asInstanceOf[Rule]
	       var B:Rule = policy.subpolicies(1).asInstanceOf[Rule]
	       var newrule = new Rule(A.id)(Deny,A.condition & (!B.condition),List[ObligationAction]())
	       var subpolicies:List[AbstractPolicy] = List(newrule,B)
	       newpolicy = new Policy(policy.id)(policy.target,PermitOverrides,subpolicies)
	     }
	   }else if(ca == FirstApplicable && policy.pca == PermitOverrides){
	     if(policy.subpolicies(0).asInstanceOf[Rule].effect == Permit){
	       newpolicy = new Policy(policy.id)(policy.target,FirstApplicable,policy.subpolicies)
	     }else{
	       newpolicy = new Policy(policy.id)(policy.target,FirstApplicable,policy.subpolicies.reverse)
	     }
	     
	   }else if(ca == FirstApplicable && policy.pca == DenyOverrides){
	     if(policy.subpolicies(0).asInstanceOf[Rule].effect == Deny){
	       newpolicy = new Policy(policy.id)(policy.target,FirstApplicable,policy.subpolicies)
	     }else{
	       newpolicy = new Policy(policy.id)(policy.target,FirstApplicable,policy.subpolicies.reverse)
	     }
	   }
	  newpolicy.parent = policy.parent
	  newpolicy.parent match {
	    case Some(x) => x.subpolicies = replace(x.subpolicies,policy,newpolicy)
	    case None => 
	  }
	  return newpolicy
	}
	
	def combinePolicies(child : Policy, parent : Policy) : Policy = {
	  val target = child.target
	  var subpolicies = List[AbstractPolicy]()
	  for(c <- child.subpolicies.reverse){
	    var r = c.asInstanceOf[Rule]
	    r =  new Rule(c.id)(r.effect,target & r.condition, List[ObligationAction]())
	    r.parent = Some(parent)
	    subpolicies ::= r
	  }
	  subpolicies = combineChildren(parent,child,subpolicies)
      parent.subpolicies = subpolicies
	  return parent
	}
	
	def combineChildren(parent: Policy, child: Policy, rules : List[AbstractPolicy]) : List[AbstractPolicy] = {
	  var resultList = List[AbstractPolicy]()
	  for(c <- parent.subpolicies.reverse){
	    if(c == child)
	      resultList :::= rules
	    else
	      resultList ::= c
	    c.parent = Some(parent)
	  }
	  return resultList
	}
	
	
	/***************************************************************************
	****************************************************************************
	********************HELPER  FUNCTIONS NORMALISATION*************************
	****************************************************************************
	****************************************************************************/
	
	def convertToSentence(condition: Expression) : Sentence =  {
	  condition match {
	    case And(x,y) => return new ComplexSentence(Connective.get("&"),convertToSentence(x),convertToSentence(y))
	    case Or(x,y) => return new ComplexSentence(Connective.get("|"),convertToSentence(x),convertToSentence(y))
	    case Not(x) => return new ComplexSentence(Connective.get("~"),convertToSentence(x))
	    case x => {propMap+=(prop+index -> x);index+=1;return new PropositionSymbol(prop+(index-1))}
	  }
	}
	
	def revertToCondition(sentence: Sentence) : Expression = {
	  sentence.getConnective() match {
	    case Connective.AND => return And(revertToCondition(sentence.getSimplerSentence(0)),revertToCondition(sentence.getSimplerSentence(1)))
	    case Connective.OR => return Or(revertToCondition(sentence.getSimplerSentence(0)),revertToCondition(sentence.getSimplerSentence(1)))
	    case Connective.NOT => return Not(revertToCondition(sentence.getSimplerSentence(0)))
	    case null => return propMap.get(sentence.asInstanceOf[PropositionSymbol].getSymbol()).getOrElse(null)
	    case _ => return null
	  }
	}
	
	val prop = "a"
	var index = 0
	var propMap = Map[String,Expression]()
	
	/***************************************************************************
	****************************************************************************
	********************HELPER  FUNCTIONS EXPANSION*****************************
	****************************************************************************
	****************************************************************************/
	
	def canBeSplit(policy : Policy) : Boolean = {
	  //TODO implement
	  return true
	}
	
	def splitPolicy(policy : Policy, atts: Set[Attribute]) : Policy = {
	  var e1 = policy.subpolicies(0).asInstanceOf[Rule].effect
	  var e2 = policy.subpolicies(1).asInstanceOf[Rule].effect
	  var commoncond = findCommon(policy.subpolicies(0).asInstanceOf[Rule],policy.subpolicies(1).asInstanceOf[Rule],atts)
	  var cond1 = policy.subpolicies(0).asInstanceOf[Rule].condition
	  var cond2 = policy.subpolicies(1).asInstanceOf[Rule].condition
	  var newRule = new Rule("newRule"+ruleIndex)(e1,commoncond,List.empty)
	  newRule.parent = Some(policy)
	  ruleIndex+=1
	  var newSubrule1 = new Rule("newRule"+ruleIndex)(e1,removeCommon(cond1,commoncond),List.empty)
	  var newSubrule2 = new Rule("newRule"+(ruleIndex+1))(e2,removeCommon(cond2,commoncond),List.empty)
	  ruleIndex += 2
	  var newsubs = List(newSubrule1,newSubrule2)
	  var newPol = new Policy("newPol"+policyIndex)(AlwaysTrue,FirstApplicable,newsubs,List.empty)
	  newPol.parent = Some(policy)
	  var newpsubs = List(newRule,newPol)
	  policy.subpolicies = newpsubs
	  return newPol
	}
	
	def splitRules(policy: Policy, atts: Set[Attribute]) : Policy = {
	  //TODO implement
	  return policy
	}
	
	def getAttributes(policy : Policy) : Set[Attribute] = {
	  //TODO implement
	  return null
	}
	
	def findCommon(r1 : Rule, r2: Rule, atts: Set[Attribute]) : Expression = {
	  var c1 = r1.condition
	  var c2 = r2.condition
	  var s1 = splitOr(c1)
	  var s2 = splitOr(c2)
	  var common = s1.&(s2)
	  var result:Expression = null
	  var max = 0
	  for(c <- common) {
	    var n = nbKnownAttributes(c,atts)
	    if(n > max) {
	      max = n
	      result = c
	    }
	  }
	  return result
	}
	
	def removeCommon(condition : Expression, common: Expression) : Expression = {
	 //TODO implement
	  return null
	}
	
	def splitOr(condition : Expression) : Set[Expression] = {
	  //TODO implement
	  return null
	}
	
	def nbKnownAttributes(condition : Expression, atts: Set[Attribute]) : Int = {
	  //TODO implement
	  return 0
	}
	
	var ruleIndex = 0
	var policyIndex = 0

}
var Nothing = { tag: "Nothing" };

function Just(x) {
    return { tag: "Just", arg: x };
}

function sequence(mxs) {
    var xs = [];
    
    for (var i = 0; i < mxs.length; i++) {
        if (mxs[i].tag === "Nothing") {
            return Nothing;
        } else if (mxs[i].tag === "Just") {
            xs.push(mxs[i].arg);
        }
    }
    
    return Just(xs);
}

function unzip(xys) {
    var xs = [];
    var ys = [];
    
    for (var i = 0; i < xys.length; i++) {
        xs.push(xys[i][0]);
        ys.push(xys[i][1]);
    }
    
    return [xs,ys]
}

function lookup(x,xys) {
    for (var i = 0; i < xys.length; i++) {
        if (xys[i][0] === x) {
            return Just(xys[i][1]);
        }
    }
    
    return Nothing;
}

function eq(x,y) {
  if (x instanceof Array && y instanceof Array) {
    if (x.length != y.length) { return false; }
    
    for (var i = 0; i < x.length; i++) {
      if (!eq(x[i], y[i])) { return false; }
    }
    
    return true;
  } else if (x.tag === y.tag) {
    if (x.args && y.args) {
      return eq(x.args,y.args);
    } else if (x.arg && y.arg) {
      return eq(x.arg,y.arg);
    } else if (!x.arg && !y.arg) {
      return true;
    } else {
      return false;
    }
  }
}


function ProofTree(c,ps) {
    return { tag: "ProofTree", args: [c,ps] };
}


function findProof(j) {
    var mjs = decompose(j);
    
    if (mjs.tag === "Nothing") {
        return Nothing;
    } else if (mjs.tag === "Just") {
        var js = mjs.arg[0]
        var f = mjs.arg[1]
        var mtns = sequence(js.map(j => findProof(j)));
        
        if (mtns.tag === "Nothing") {
            return Nothing;
        } else if (mtns.tag === "Just") {
            var tsns = unzip(mtns.arg);
            var mn = f(tsns[1]);
            
            if (mn.tag === "Nothing") {
                return Nothing;
            } else if (mn.tag === "Just") {
                return Just([ProofTree(j, tsns[0]), mn.arg]);
            }
        }
    }
}



//
// Judgments 
//

function Check(g,m,a) {
    return { tag: "Check", args: [g,m,a] };
}

function Synth(g,m) {
    return { tag: "Synth", args: [g,m] };
}



//
// Types
//

var Nat = { tag: "Nat" };

function Prod(a,b) {
    return { tag: "Prod", args: [a,b] };
}

function Arr(a,b) {
    return { tag: "Arr", args: [a,b] };
}



//
// Programs
//

function Var(x) {
    return { tag: "Var", arg: x };
}

function Ann(m,a) {
    return { tag: "Ann", args: [m,a] };
}

var Zero = { tag: "Zero" };

function Suc(m) {
  return { tag: "Suc", arg: m };
}

function Pair(m,n) {
    return { tag: "Pair", args: [m,n] };
}

function Fst(m) {
    return { tag: "Fst", arg: m };
}

function Snd(m) {
    return { tag: "Snd", arg: m };
}

function Lam(x,m) {
    return { tag: "Lam", args: [x,m] };
}

function App(m,n) {
    return { tag: "App", args: [m,n] };
}




function decomposeCheck(g,m,a) {
    if (m.tag === "Zero" && a.tag === "Nat") {
        return Just([]);
    } else if (m.tag === "Suc" && a.tag === "Nat") {
        return Just([Check(g,m.arg,Nat)]);
    } else if (m.tag === "Pair" && a.tag === "Prod") {
        return Just([ [Check(g, m.args[0], a.args[0]),
                       Check(g, m.args[1], a.args[1])],
                      as => Just(undefined)]);
    } else if (m.tag === "Lam" && a.tag === "Arr") {
        return Just([ [Check([[m.args[0], a.args[0]]].concat(g),
                             m.args[1],
                             a.args[1])],
                      as => Just(undefined)]);
    } else {
        return Just([ [Synth(g,m,a)],
                      as => eq(a,as[0]) ? Just(undefined) : Nothing ])
    }
}

function decomposeSynth(g,m) {
    if (m.tag === "Var") {
        var ma = lookup(m.arg, g);
        
        if (ma.tag === "Nothing") {
            return Nothing;
        } else if (ma.tag === "Just") {
            return Just([[], as => Just(ma.arg)]);
        }
    } else if (m.tag === "Ann") {
        return Just([ [Check(g, m.args[0], m.args[1])],
                      as => Just(m.args[1]) ]);
    } else if (m.tag === "Fst") {
        return Just([ [Synth(g, m.arg)],
                      as => as[0].tag === "Prod" ?
                              Just(as[0].args[0]) :
                              Nothing ]);
    } else if (m.tag === "Snd") {
        return Just([ [Synth(g, m.arg)],
                      as => as[0].tag === "Prod" ?
                              Just(as[0].args[1]) :
                              Nothing ]);
    } else if (m.tag === "App") {
        return Just([ [Synth(g, m.args[0]), Synth(g, m.args[1])],
                      as => as[0].tag === "Arr" &&
                              eq(as[0].args[0], as[1]) ?
                                Just(as[0].args[1]) :
                                Nothing ]);
    } else {
        return Nothing;
    }
}

function decompose(j) {
    if (j.tag === "Check") {
        return decomposeCheck(j.args[0], j.args[1], j.args[2]);
    } else if (j.tag === "Synth") {
        return decomposeSynth(j.args[0], j.args[1]);
    }
}
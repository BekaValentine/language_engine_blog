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
            return Just([ProofTree(j, tsns[0]), f(tsns[1])]);
        }
    }
}


var Zero = { tag: "Zero" };

function Suc(n) {
    return { tag: "Suc", arg: n };
}

function Plus12(x,y) {
    return { tag: "Plus12", args: [x,y] };
}

function Plus13(x,z) {
    return { tag: "Plus13", args: [x,z] };
}

function decomposePlus12(x,y) {
    if (x.tag === "Zero") {
        return Just([[], zs => y]);
    } else if (x.tag === "Suc") {
        return Just([[Plus12(x.arg,y)], zs => Suc(zs[0])]);
    }
}

function decomposePlus13(x,z) {
    if (x.tag === "Zero") {
        return Just([[], xs => z]);
    } else if (x.tag === "Suc" && z.tag === "Suc") {
        return Just([[Plus13(x.arg, z.arg)], xs => xs[0]]);
    } else {
        return Nothing;
    }
}

function decompose(j) {
    if (j.tag === "Plus12") {
        return decomposePlus12(j.args[0], j.args[1]);
    } else if (j.tag === "Plus13") {
        return decomposePlus13(j.args[0], j.args[1]);
    }
}
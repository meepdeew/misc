function main() {
    // evex("2 +-31");
    // evex("-1--1");
    // evex("-1+-1");
    // evex("-2*-3+-1");
    // evex("1 + -2 + 3");
}

function evex(exp) {
    // console.log("##### Starting: "+exprStr+" #####");
    var asd1 = exp.split("");
    // console.log("split: ");
    // console.log(asd1);
    var asd2 = rmSpaces(asd1);
    // console.log("rm sp: ");
    // console.log(asd2);
    var asd3 = joinAsNecessary(asd2);
    // console.log("joind: ");
    // console.log(asd3);
    var asd4 = evexHelperMulDiv(asd3);
    // console.log("multd: ");
    // console.log(asd4);
    var asd5 = evexHelperAddSub(asd4);
    // console.log("added: ");
    // console.log(asd5);
    return asd5;
}

function countMulDivOps(lst) {
    var count = 0;
    for(var i=0; i<lst.length; i++) {
	var cur = lst[i];
	if (isMD(cur)) {
	    count++;
	}
    }
    return count;
}

function consOnto(){}

function isMD(str) {
    return str==="*"||str==="/";
}
function evexMdOnce(lst) {
    var res = [];
    res.push(lst[0]);
    var done = false;
    // find first op and eval L and R
    for(var i=1; i<lst.length; i++) {
	var lastEl = res[res.length-1];
	var cur = lst[i];
	// console.log("res: ");
	// console.log(res);
	// console.log("looping on ",cur);
	// if (i+1 >= lst.length) {
	//     break;
	// }
	var next = lst[i+1];

	if (done) {
	    res.push(cur);
	}
	if (!done && isMD(cur) && !isOp(lastEl) && !isOp(next)) {
	    res.pop();
	    var v;
	    if (cur === "*") {
		v = parseInt(lastEl) * parseInt(next);
	    }
	    else if (cur === "/") {
		v = Math.floor(parseInt(lastEl) / parseInt(next));
	    }
	    res.push(""+v);
	    i++;
	    done = true;
	}
	if (!done) {
	    res.push(cur);
	}

    }
    // console.log(res);
    return res;
}

function evexHelperMulDiv(lst){
    var times = countMulDivOps(lst);
    // console.log("times");
    // console.log(times);
    var thing = lst
    for(var i=0; i<times; i++) {
	// console.log(thing);
	thing = evexMdOnce(thing);
    }
    // console.log(thing);
    return thing;
}

function evexHelperAddSub(lst, res, sign){
    // TODO:
    var res = 0;

    res = lst[0];
    for (var i=1; i<lst.length; i++) {
	var cur = lst[i];
	if (isOp(cur)) {
	    if (cur === "+") {
		res = parseInt(res) + parseInt(lst[i+1]);
	    }
	    if (cur === "-") {
		res = parseInt(res) - parseInt(lst[i+1]);
	    }
	}
    }
    // console.log(res);
    return res;
}

function joinAsNecessary(lst){
    var asd1 = joinDigits(lst);
    var asd2 = joinNegativeNums(asd1);
    return asd2;
}

function isNum(nStr) {
    return nStr >= "0" && nStr <= "9";
}

function joinDigits(lst){
    // join contiguous digits
    var res = [];

    var i=0;
    while (i < lst.length) {
	var cur = lst[i];
	if (isNum(cur)) {
	    // find last
	    var j=i+1;
	    while (j < lst.length && isNum(lst[j])) {
		j++;
	    }
	    var multiDigit = lst.slice(i,j);
	    // console.log("mdig");
	    // console.log(multiDigit);
	    res.push(multiDigit.join(""));
	    i=j;
	}
	else {
	    res.push(cur);
	    i++
	}
    }

    for (var i=0; i<lst.length; i++) {
	
	var cur = lst[i];

	lastChar = cur;
    }
    // console.log(res);
    return res;
}

function isOp(str) {
    return (str === "+" || str === "*" || str === "-" || str === "/");
}

function joinNegativeNums(lst){
    var res = [];
    for (var i=0; i<lst.length; i++) {
	var cur = lst[i];
	var next = lst[i+1];
	var prior;
	if (i===0 && cur === "-" && isNum(next)) {
	    // join v0+v1, move to
	    var combo = cur + next;
	    res.push(combo);
	    prior = combo;
	    i++;
	}
	else if (isOp(prior) && cur === "-" && isNum(next)) {
	    var combo = cur + next;
	    res.push(combo);
	    prior = combo;
	    i++;
	}
	else {
	    res.push(cur);
	    prior = cur;
	}
    }
    // console.log(res);
    return res;
}

function rmSpaces(lst){
    return lst.filter((item, idx) => {
	return item !== ' ';
    });
}


// main();


// var inp = [ '23', '/', '23', '+', '3', '*', '4', '-', '4', '/', '3' ];
// var rez = evexHelperMulDiv(inp);
// joinDigits(["2","-","3","4"]); //["2","-","34"]
// joinDigits(["2", "3","-","4"]); //["23","-","4"]

// joinNegativeNums(["2","+","-","3"]);
// joinNegativeNums(["-","4","+","6"]);
// joinNegativeNums(["-","1","-","-","1"]);
// joinNegativeNums(["1","+","2","+","3"]);


// evexMdOnce(["2","*","3"]);//[6]
// evexMdOnce(["2","*","3","*","4"]);//
// evexMdOnce(["3","/","2","*","4"]);//
// var tmp1 = ["a", "b", "c", "d", "e", "f", "g"];
// console.log(tmp1.slice(1,3));


// console.log("1");
// console.log(evex("1 + 2 * 3") === 7);
// console.log("2");
// console.log(evex("1 - 4 / 3") === 0);
// console.log("3"); //FALSE
// console.log(evex("-123") === -123);
// console.log("4");
// console.log(evex("1 / 3 - 1 - 23") === -24);
// console.log("5"); // FALSE
// console.log(evex("1 * 2 * 3 * 4 / 5") === 4);
// console.log("6");
// console.log(evex("1 + 123 / 123") === 2);
// console.log("7");
// console.log(evex("3 * 3 / 3 + 12") === 15);
// console.log("8");
// console.log(evex("2 * 3 + 4 * 5") === 26);
// console.log("9");
// console.log(evex("23 / 23 + 3 * 4 - 4 / 3") === 12);
// console.log("10"); // FALSE
// console.log(evex("6 / 3 / 2") === 1);
// console.log("11");
// console.log(evex("1 / 3 - -10 + 20") === 30);
// console.log("12");
// console.log(evex("1 / 5 - 10 + 20") === 10);


// console.log("3"); //FALSE
// console.log(evex("-123") == -123);
// console.log(evex("-123"));
// console.log("5"); // FALSE
// console.log(evex("1 * 2 * 3 * 4 / 5") == 4);
// console.log(evex("1 * 2 * 3 * 4 / 5"));
// console.log("10"); // FALSE
// console.log(evex("6 / 3 / 2") == 1);
// console.log(evex("6 / 3 / 2"));

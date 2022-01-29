var gen = 0;
var interval = 0;
var dostop = false;
var ms = null;
var rid = null;
var stepping = false;

function init(millisecs) {
	interval = millisecs
	document.getElementById("outval").value = millisecs
	document.getElementById("start").focus()
}

function decgen() { /* prevent negative zero */
	if (gen > 0) {
		gen -= 1;
	} else if (gen === -0) {
		gen = -gen;
	}
}

function start() {
	cancelAnimationFrame(rid);
	gen = 0;
	dostop = false;
	stepping = false;
	rid = requestAnimationFrame(autostep);
}

function autostep(timestamp) {
	if (ms == null) {
		ms = timestamp;
	}
	if (dostop == true || stepping == true) {
		cancelAnimationFrame(rid);
		return;
	}
	if (timestamp - ms > interval) {
		ms = timestamp;
		gen += 1;
		document.getElementById("image").src = "./images/life" + gen + ".jpg";
	}
	rid = requestAnimationFrame(autostep);
}

function stop() {
	dostop = true;
	stepping = true;
	cancelAnimationFrame(rid);
}

function contin() {
	dostop = false;
	stepping = false;
	rid = requestAnimationFrame(autostep);
}

function forward() {
	dostop = false;
	stepping = true;
	gen += 1;
	document.getElementById("image").src = "./images/life" + gen + ".jpg";
	rid = requestAnimationFrame(autostep);
}

function backward() {
	dostop = false;
	stepping = true;
	decgen();
	document.getElementById("image").src = "./images/life" + gen + ".jpg";
	rid = requestAnimationFrame(autostep);
}

function setinterval(num) {
	interval = parseInt(num);
	return interval;
}

function fini() {
	gen -= 1
	document.getElementById("image").src = "./images/life" + gen + ".jpg";
	cancelAnimationFrame(rid);
}

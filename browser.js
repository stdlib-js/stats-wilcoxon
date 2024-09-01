// Copyright (c) 2024 The Stdlib Authors. License is Apache-2.0: http://www.apache.org/licenses/LICENSE-2.0
var r,e;r=this,e=function(){"use strict";var r="function"==typeof Object.defineProperty?Object.defineProperty:null,e=Object.defineProperty;function n(r){return"number"==typeof r}function t(r){var e,n="";for(e=0;e<r;e++)n+="0";return n}function i(r,e,n){var i=!1,o=e-r.length;return o<0||(function(r){return"-"===r[0]}(r)&&(i=!0,r=r.substr(1)),r=n?r+t(o):t(o)+r,i&&(r="-"+r)),r}var o=String.prototype.toLowerCase,a=String.prototype.toUpperCase;function u(r){var e,t,u;switch(r.specifier){case"b":e=2;break;case"o":e=8;break;case"x":case"X":e=16;break;default:e=10}if(t=r.arg,u=parseInt(t,10),!isFinite(u)){if(!n(t))throw new Error("invalid integer. Value: "+t);u=0}return u<0&&("u"===r.specifier||10!==e)&&(u=4294967295+u+1),u<0?(t=(-u).toString(e),r.precision&&(t=i(t,r.precision,r.padRight)),t="-"+t):(t=u.toString(e),u||r.precision?r.precision&&(t=i(t,r.precision,r.padRight)):t="",r.sign&&(t=r.sign+t)),16===e&&(r.alternate&&(t="0x"+t),t=r.specifier===a.call(r.specifier)?a.call(t):o.call(t)),8===e&&r.alternate&&"0"!==t.charAt(0)&&(t="0"+t),t}var f=Math.abs,c=String.prototype.toLowerCase,l=String.prototype.toUpperCase,s=String.prototype.replace,p=/e\+(\d)$/,h=/e-(\d)$/,g=/^(\d+)$/,y=/^(\d+)e/,d=/\.0$/,v=/\.0*e/,w=/(\..*[^0])0*e/;function b(r){var e,t,i=parseFloat(r.arg);if(!isFinite(i)){if(!n(r.arg))throw new Error("invalid floating-point number. Value: "+t);i=r.arg}switch(r.specifier){case"e":case"E":t=i.toExponential(r.precision);break;case"f":case"F":t=i.toFixed(r.precision);break;case"g":case"G":f(i)<1e-4?((e=r.precision)>0&&(e-=1),t=i.toExponential(e)):t=i.toPrecision(r.precision),r.alternate||(t=s.call(t,w,"$1e"),t=s.call(t,v,"e"),t=s.call(t,d,""));break;default:throw new Error("invalid double notation. Value: "+r.specifier)}return t=s.call(t,p,"e+0$1"),t=s.call(t,h,"e-0$1"),r.alternate&&(t=s.call(t,g,"$1."),t=s.call(t,y,"$1.e")),i>=0&&r.sign&&(t=r.sign+t),t=r.specifier===l.call(r.specifier)?l.call(t):c.call(t)}function m(r){var e,n="";for(e=0;e<r;e++)n+=" ";return n}var E=String.fromCharCode,j=Array.isArray;function x(r){return r!=r}function A(r){var e={};return e.specifier=r.specifier,e.precision=void 0===r.precision?1:r.precision,e.width=r.width,e.flags=r.flags||"",e.mapping=r.mapping,e}function T(r){var e,n,t,o,a,f,c,l,s,p,h,g,y;if(!j(r))throw new TypeError("invalid argument. First argument must be an array. Value: `"+r+"`.");for(f="",c=1,l=0;l<r.length;l++)if("string"==typeof(t=r[l]))f+=t;else{if(e=void 0!==t.precision,!(t=A(t)).specifier)throw new TypeError("invalid argument. Token is missing `specifier` property. Index: `"+l+"`. Value: `"+t+"`.");for(t.mapping&&(c=t.mapping),n=t.flags,s=0;s<n.length;s++)switch(o=n.charAt(s)){case" ":t.sign=" ";break;case"+":t.sign="+";break;case"-":t.padRight=!0,t.padZeros=!1;break;case"0":t.padZeros=n.indexOf("-")<0;break;case"#":t.alternate=!0;break;default:throw new Error("invalid flag: "+o)}if("*"===t.width){if(t.width=parseInt(arguments[c],10),c+=1,x(t.width))throw new TypeError("the argument for * width at position "+c+" is not a number. Value: `"+t.width+"`.");t.width<0&&(t.padRight=!0,t.width=-t.width)}if(e&&"*"===t.precision){if(t.precision=parseInt(arguments[c],10),c+=1,x(t.precision))throw new TypeError("the argument for * precision at position "+c+" is not a number. Value: `"+t.precision+"`.");t.precision<0&&(t.precision=1,e=!1)}switch(t.arg=arguments[c],t.specifier){case"b":case"o":case"x":case"X":case"d":case"i":case"u":e&&(t.padZeros=!1),t.arg=u(t);break;case"s":t.maxWidth=e?t.precision:-1,t.arg=String(t.arg);break;case"c":if(!x(t.arg)){if((a=parseInt(t.arg,10))<0||a>127)throw new Error("invalid character code. Value: "+t.arg);t.arg=x(a)?String(t.arg):E(a)}break;case"e":case"E":case"f":case"F":case"g":case"G":e||(t.precision=6),t.arg=b(t);break;default:throw new Error("invalid specifier: "+t.specifier)}t.maxWidth>=0&&t.arg.length>t.maxWidth&&(t.arg=t.arg.substring(0,t.maxWidth)),t.padZeros?t.arg=i(t.arg,t.width||t.precision,t.padRight):t.width&&(t.arg=(p=t.arg,h=t.width,g=t.padRight,y=void 0,(y=h-p.length)<0?p:p=g?p+m(y):m(y)+p)),f+=t.arg||"",c+=1}return f}var O=/%(?:([1-9]\d*)\$)?([0 +\-#]*)(\*|\d+)?(?:(\.)(\*|\d+)?)?[hlL]?([%A-Za-z])/g;function N(r){var e={mapping:r[1]?parseInt(r[1],10):void 0,flags:r[2],width:r[3],precision:r[5],specifier:r[6]};return"."===r[4]&&void 0===r[5]&&(e.precision="1"),e}function _(r){var e,n,t,i;for(n=[],i=0,t=O.exec(r);t;)(e=r.slice(i,O.lastIndex-t[0].length)).length&&n.push(e),n.push(N(t)),i=O.lastIndex,t=O.exec(r);return(e=r.slice(i)).length&&n.push(e),n}function S(r){var e,n;if("string"!=typeof r)throw new TypeError(S("invalid argument. First argument must be a string. Value: `%s`.",r));for(e=[_(r)],n=1;n<arguments.length;n++)e.push(arguments[n]);return T.apply(null,e)}var V,k=Object.prototype,L=k.toString,F=k.__defineGetter__,P=k.__defineSetter__,U=k.__lookupGetter__,I=k.__lookupSetter__;V=function(){try{return r({},"x",{}),!0}catch(r){return!1}}()?e:function(r,e,n){var t,i,o,a;if("object"!=typeof r||null===r||"[object Array]"===L.call(r))throw new TypeError(S("invalid argument. First argument must be an object. Value: `%s`.",r));if("object"!=typeof n||null===n||"[object Array]"===L.call(n))throw new TypeError(S("invalid argument. Property descriptor must be an object. Value: `%s`.",n));if((i="value"in n)&&(U.call(r,e)||I.call(r,e)?(t=r.__proto__,r.__proto__=k,delete r[e],r[e]=n.value,r.__proto__=t):r[e]=n.value),o="get"in n,a="set"in n,i&&(o||a))throw new Error("invalid argument. Cannot specify one or more accessors and a value or writable attribute in the property descriptor.");return o&&F&&F.call(r,e,n.get),a&&P&&P.call(r,e,n.set),r};var M=V;function z(r,e,n){M(r,e,{configurable:!1,enumerable:!1,writable:!1,value:n})}var R=Math.floor;function W(r){return R(r)===r}var C=4294967295;function G(r){if("function"!=typeof r)throw new TypeError(S("invalid argument. Must provide a function. Value: `%s`.",r));return function(e){var n,t;if(!function(r){return null!=r&&"function"!=typeof r&&"number"==typeof r.length&&W(r.length)&&r.length>=0&&r.length<=C}(e))return!1;if(0===(n=e.length))return!1;for(t=0;t<n;t++)if(!1===r(e[t]))return!1;return!0}}function $(r){return"number"==typeof r}var H="function"==typeof Symbol&&"symbol"==typeof Symbol("foo");function B(){return H&&"symbol"==typeof Symbol.toStringTag}var Z=Object.prototype.toString,X=Object.prototype.hasOwnProperty;function Y(r,e){return null!=r&&X.call(r,e)}var q="function"==typeof Symbol?Symbol:void 0,D="function"==typeof q?q.toStringTag:"",J=B()?function(r){var e,n,t;if(null==r)return Z.call(r);n=r[D],e=Y(r,D);try{r[D]=void 0}catch(e){return Z.call(r)}return t=Z.call(r),e?r[D]=n:delete r[D],t}:function(r){return Z.call(r)},K=Number,Q=K.prototype.toString,rr=B();function er(r){return"object"==typeof r&&(r instanceof K||(rr?function(r){try{return Q.call(r),!0}catch(r){return!1}}(r):"[object Number]"===J(r)))}function nr(r){return $(r)||er(r)}z(nr,"isPrimitive",$),z(nr,"isObject",er);var tr=G(nr.isPrimitive),ir=G(nr.isObject),or=G(nr);z(or,"primitives",tr),z(or,"objects",ir);var ar=Number.POSITIVE_INFINITY,ur=K.NEGATIVE_INFINITY;function fr(r){return r<ar&&r>ur&&W(r)}function cr(r){return $(r)&&fr(r)}function lr(r){return er(r)&&fr(r.valueOf())}function sr(r){return cr(r)||lr(r)}function pr(r){return cr(r)&&r>=0}function hr(r){return lr(r)&&r.valueOf()>=0}function gr(r){return pr(r)||hr(r)}z(sr,"isPrimitive",cr),z(sr,"isObject",lr),z(gr,"isPrimitive",pr),z(gr,"isObject",hr);var yr=9007199254740991;function dr(r){return null!==r&&"object"==typeof r&&pr(r.length)&&r.length<=yr&&"number"==typeof r.BYTES_PER_ELEMENT&&"number"==typeof r.byteOffset&&"number"==typeof r.byteLength}function vr(r,e,n){M(r,e,{configurable:!1,enumerable:!0,writable:!1,value:n})}var wr=Array.isArray?Array.isArray:function(r){return"[object Array]"===J(r)};function br(r){return"object"==typeof r&&null!==r&&!wr(r)}var mr=/./;function Er(r){return"boolean"==typeof r}var jr=Boolean,xr=Boolean.prototype.toString,Ar=B();function Tr(r){return"object"==typeof r&&(r instanceof jr||(Ar?function(r){try{return xr.call(r),!0}catch(r){return!1}}(r):"[object Boolean]"===J(r)))}function Or(r){return Er(r)||Tr(r)}z(Or,"isPrimitive",Er),z(Or,"isObject",Tr);var Nr="object"==typeof self?self:null,_r="object"==typeof window?window:null,Sr="object"==typeof globalThis?globalThis:null,Vr=function(r){if(arguments.length){if(!Er(r))throw new TypeError(S("invalid argument. Must provide a boolean. Value: `%s`.",r));if(r)return new Function("return this;")()}if(Sr)return Sr;if(Nr)return Nr;if(_r)return _r;throw new Error("unexpected error. Unable to resolve global object.")}(),kr=Vr.document&&Vr.document.childNodes,Lr=Int8Array;function Fr(){return/^\s*function\s*([^(]*)/i}var Pr=/^\s*function\s*([^(]*)/i;function Ur(r){return null!==r&&"object"==typeof r}function Ir(r){var e,n,t,i;if(("Object"===(n=J(r).slice(8,-1))||"Error"===n)&&r.constructor){if("string"==typeof(t=r.constructor).name)return t.name;if(e=Pr.exec(t.toString()))return e[1]}return Ur(i=r)&&(i._isBuffer||i.constructor&&"function"==typeof i.constructor.isBuffer&&i.constructor.isBuffer(i))?"Buffer":n}z(Fr,"REGEXP",Pr),z(Ur,"isObjectLikeArray",function(r){if("function"!=typeof r)throw new TypeError(S("invalid argument. Must provide a function. Value: `%s`.",r));return function(e){var n,t;if(!wr(e))return!1;if(0===(n=e.length))return!1;for(t=0;t<n;t++)if(!1===r(e[t]))return!1;return!0}}(Ur));var Mr="function"==typeof mr||"object"==typeof Lr||"function"==typeof kr?function(r){return Ir(r).toLowerCase()}:function(r){var e;return null===r?"null":"object"==(e=typeof r)?Ir(r).toLowerCase():e};function zr(r){return"function"===Mr(r)}var Rr,Wr=Object,Cr=Object.getPrototypeOf;Rr=zr(Object.getPrototypeOf)?Cr:function(r){var e=function(r){return r.__proto__}(r);return e||null===e?e:"[object Function]"===J(r.constructor)?r.constructor.prototype:r instanceof Object?Object.prototype:null};var Gr=Rr,$r=Object.prototype;function Hr(r){var e;return!!br(r)&&(e=function(r){return null==r?null:(r=Wr(r),Gr(r))}(r),!e||!Y(r,"constructor")&&Y(e,"constructor")&&zr(e.constructor)&&"[object Function]"===J(e.constructor)&&Y(e,"isPrototypeOf")&&zr(e.isPrototypeOf)&&(e===$r||function(r){var e;for(e in r)if(!Y(r,e))return!1;return!0}(r)))}function Br(r){return"object"==typeof r&&null!==r&&"number"==typeof r.length&&W(r.length)&&r.length>=0&&r.length<=yr}function Zr(r){return"string"==typeof r}var Xr=String.prototype.valueOf,Yr=B();function qr(r){return"object"==typeof r&&(r instanceof String||(Yr?function(r){try{return Xr.call(r),!0}catch(r){return!1}}(r):"[object String]"===J(r)))}function Dr(r){return Zr(r)||qr(r)}function Jr(r){return r!=r}function Kr(r){return $(r)&&Jr(r)}function Qr(r){return er(r)&&Jr(r.valueOf())}function re(r){return Kr(r)||Qr(r)}function ee(r,e,n){var t,i,o;if(!Br(r)&&!Zr(r))throw new TypeError(S("invalid argument. First argument must be array-like. Value: `%s`.",r));if(arguments.length<2)throw new Error("insufficient arguments. Must provide a search value.");if(arguments.length>2){if(!cr(n))throw new TypeError(S("invalid argument. Third argument must be an integer. Value: `%s`.",n));(i=n)<0&&(i=0)}else i=0;if(Zr(r)){if(!Zr(e))throw new TypeError(S("invalid argument. Second argument must be a string. Value: `%s`.",e));return-1!==r.indexOf(e,i)}if(t=r.length,Kr(e)){for(o=i;o<t;o++)if(Kr(r[o]))return!0;return!1}for(o=i;o<t;o++)if(r[o]===e)return!0;return!1}z(Dr,"isPrimitive",Zr),z(Dr,"isObject",qr),z(re,"isPrimitive",Kr),z(re,"isObject",Qr);var ne=["min","max","average","dense","ordinal"],te=["last","first","remove"];function ie(r,e){var n,t,i,o,a,u,f,c,l,s,p,h,g,y,d,v,w,b;if(!Br(r))throw new TypeError(S("invalid argument. First argument must be an array-like object. Value: `%s`.",r));if(g={},arguments.length>1&&(d=function(r,e){return br(e)?Y(e,"encoding")&&(r.encoding=e.encoding,!wr(r.encoding))?new TypeError(S("invalid option. `%s` option must be an array. Option: `%s`.","encoding",r.encoding)):!Y(e,"method")||(r.method=e.method,Zr(r.method)&&ee(ne,r.method))?!Y(e,"missing")||(r.missing=e.missing,Zr(r.missing)&&ee(te,r.missing))?null:new TypeError(S('invalid option. `%s` option must be one of the following: "%s". Option: `%s`.',"missing",te.join('", "'),r.missing)):new TypeError(S('invalid option. `%s` option must be one of the following: "%s". Option: `%s`.',"method",ne.join('", "'),r.method)):new TypeError(S("invalid argument. Options argument must be an object. Value: `%s`.",e))}(g,e),d))throw d;for(p=g.method||"average",u=g.encoding||[null,NaN],l=g.missing||"last",v=r.length,y=[],w=0;w<v;w++)ee(u,r[w])||y.push(r[w]);if(n=function(r,e){var n,t,i;for(n=r.length,t=new Array(n),i=0;i<n;i++)t[i]=ee(e,r[i]);return t}(r,u),v=y.length,o=0,h=new Array(v),c=function(r){var e,n;for(e=new Array(r.length),n=0;n<r.length;n++)e[n]=n;return e.sort((function(e,n){return function(r,e){return r<e?-1:r>e?1:0}(r[e],r[n])}))}(y),"ordinal"===p)for(w=0;w<v;w++)h[c[w]]=w+1;else for(t=0,w=0;w<v;w++)if(f=w+1,w===v-1||y[c[w]]!==y[c[f]]){switch(p){case"min":s=f-t;break;case"max":s=f;break;case"dense":s=f-t-o,o+=t;break;default:s=f-.5*t}for(b=w-t;b<f;b++)h[c[b]]=s;t=0}else t+=1;if("first"===l){for(i=function(r){var e,n,t;for(e=r.length,n=0,t=0;t<e;t++)n+=r[t];return n}(n),b=1,a=new Array(n.length),w=0;w<n.length;w++)n[w]?(a[w]=b,b+=1):a[w]=h.shift()+i;return a}if("last"===l){for(a=new Array(n.length),w=0;w<n.length;w++)n[w]?a[w]=w+h.length+1:a[w]=h.shift();return a}return h}var oe=Math.ceil;function ae(r){return r<0?oe(r):R(r)}var ue=1023,fe=1023,ce=-1023,le=-1074;function se(r){return r===ar||r===ur}var pe,he=2147483648,ge=2147483647,ye="function"==typeof Uint32Array,de="function"==typeof Uint32Array?Uint32Array:null,ve="function"==typeof Uint32Array?Uint32Array:void 0;pe=function(){var r,e,n;if("function"!=typeof de)return!1;try{e=new de(e=[1,3.14,-3.14,4294967296,4294967297]),n=e,r=(ye&&n instanceof Uint32Array||"[object Uint32Array]"===J(n))&&1===e[0]&&3===e[1]&&4294967293===e[2]&&0===e[3]&&1===e[4]}catch(e){r=!1}return r}()?ve:function(){throw new Error("not implemented")};var we,be=pe,me="function"==typeof Float64Array,Ee="function"==typeof Float64Array?Float64Array:null,je="function"==typeof Float64Array?Float64Array:void 0;we=function(){var r,e,n;if("function"!=typeof Ee)return!1;try{e=new Ee([1,3.14,-3.14,NaN]),n=e,r=(me&&n instanceof Float64Array||"[object Float64Array]"===J(n))&&1===e[0]&&3.14===e[1]&&-3.14===e[2]&&e[3]!=e[3]}catch(e){r=!1}return r}()?je:function(){throw new Error("not implemented")};var xe,Ae=we,Te="function"==typeof Uint8Array,Oe="function"==typeof Uint8Array?Uint8Array:null,Ne="function"==typeof Uint8Array?Uint8Array:void 0;xe=function(){var r,e,n;if("function"!=typeof Oe)return!1;try{e=new Oe(e=[1,3.14,-3.14,256,257]),n=e,r=(Te&&n instanceof Uint8Array||"[object Uint8Array]"===J(n))&&1===e[0]&&3===e[1]&&253===e[2]&&0===e[3]&&1===e[4]}catch(e){r=!1}return r}()?Ne:function(){throw new Error("not implemented")};var _e,Se=xe,Ve="function"==typeof Uint16Array,ke="function"==typeof Uint16Array?Uint16Array:null,Le="function"==typeof Uint16Array?Uint16Array:void 0;_e=function(){var r,e,n;if("function"!=typeof ke)return!1;try{e=new ke(e=[1,3.14,-3.14,65536,65537]),n=e,r=(Ve&&n instanceof Uint16Array||"[object Uint16Array]"===J(n))&&1===e[0]&&3===e[1]&&65533===e[2]&&0===e[3]&&1===e[4]}catch(e){r=!1}return r}()?Le:function(){throw new Error("not implemented")};var Fe,Pe={uint16:_e,uint8:Se};(Fe=new Pe.uint16(1))[0]=4660;var Ue,Ie,Me=52===new Pe.uint8(Fe.buffer)[0];!0===Me?(Ue=1,Ie=0):(Ue=0,Ie=1);var ze={HIGH:Ue,LOW:Ie},Re=new Ae(1),We=new be(Re.buffer),Ce=ze.HIGH,Ge=ze.LOW;function $e(r,e,n,t){return Re[0]=r,e[t]=We[Ce],e[t+n]=We[Ge],e}function He(r){return $e(r,[0,0],1,0)}z(He,"assign",$e);var Be,Ze,Xe=!0===Me?1:0,Ye=new Ae(1),qe=new be(Ye.buffer);function De(r){return Ye[0]=r,qe[Xe]}!0===Me?(Be=1,Ze=0):(Be=0,Ze=1);var Je={HIGH:Be,LOW:Ze},Ke=new Ae(1),Qe=new be(Ke.buffer),rn=Je.HIGH,en=Je.LOW;function nn(r,e){return Qe[rn]=r,Qe[en]=e,Ke[0]}var tn=[0,0];function on(r,e){var n,t;return He.assign(r,tn,1,0),n=tn[0],n&=ge,t=De(e),nn(n|=t&=he,tn[1])}var an=22250738585072014e-324;function un(r){return Math.abs(r)}var fn=4503599627370496;function cn(r,e,n,t){return Jr(r)||se(r)?(e[t]=r,e[t+n]=0,e):0!==r&&un(r)<an?(e[t]=r*fn,e[t+n]=-52,e):(e[t]=r,e[t+n]=0,e)}z((function(r){return cn(r,[0,0],1,0)}),"assign",cn);var ln=2146435072,sn=2220446049250313e-31,pn=2148532223,hn=[0,0],gn=[0,0];function yn(r,e){var n,t;return 0===e||0===r||Jr(r)||se(r)?r:(cn(r,hn,1,0),r=hn[0],e+=hn[1],e+=function(r){var e=De(r);return(e=(e&ln)>>>20)-ue|0}(r),e<le?on(0,r):e>fe?r<0?ur:ar:(e<=ce?(e+=52,t=sn):t=1,He.assign(r,gn,1,0),n=gn[0],n&=pn,t*nn(n|=e+ue<<20,gn[1])))}var dn=.6931471803691238,vn=1.9082149292705877e-10,wn=1.4426950408889634,bn=709.782712893384,mn=-745.1332191019411,En=1/(1<<28),jn=-En;function xn(r){var e;return Jr(r)||r===ar?r:r===ur?0:r>bn?ar:r<mn?0:r>jn&&r<En?1+r:function(r,e,n){var t,i,o,a;return yn(1-(e-(t=r-e)*(o=t-(i=t*t)*(0===(a=i)?.16666666666666602:.16666666666666602+a*(a*(6613756321437934e-20+a*(4.1381367970572385e-8*a-16533902205465252e-22))-.0027777777777015593)))/(2-o)-r),n)}(r-(e=ae(r<0?wn*r-.5:wn*r+.5))*dn,e*vn,e)}var An=!0===Me?0:1,Tn=new Ae(1),On=new be(Tn.buffer);function Nn(r,e){return Tn[0]=r,On[An]=e>>>0,Tn[0]}var _n=1e-300,Sn=13877787807814457e-33,Vn=.8450629115104675,kn=.12837916709551256,Ln=1,Fn=-.0023621185607526594,Pn=1,Un=-.009864944034847148,In=1,Mn=-.0098649429247001,zn=1;function Rn(r){var e,n,t,i,o,a,u,f;if(Jr(r))return NaN;if(r===ar)return 0;if(r===ur)return 2;if(0===r)return 1;if(r<0?(e=!0,n=-r):(e=!1,n=r),n<.84375)return n<Sn?1-r:(i=kn+(t=r*r)*function(r){return 0===r?-.3250421072470015:r*(r*(-23763016656650163e-21*r-.005770270296489442)-.02848174957559851)-.3250421072470015}(t),o=Ln+t*function(r){return 0===r?.39791722395915535:.39791722395915535+r*(.0650222499887673+r*(.005081306281875766+r*(.00013249473800432164+-3960228278775368e-21*r)))}(t),a=i/o,r<.25?1-(r+r*a):(i=r*a,.5-(i+=r-.5)));if(n<1.25)return u=Fn+(o=n-1)*function(r){return 0===r?.41485611868374833:.41485611868374833+r*(r*(.31834661990116175+r*(r*(.035478304325618236+-.002166375594868791*r)-.11089469428239668))-.3722078760357013)}(o),f=Pn+o*function(r){return 0===r?.10642088040084423:.10642088040084423+r*(.540397917702171+r*(.07182865441419627+r*(.12617121980876164+r*(.01363708391202905+.011984499846799107*r))))}(o),e?1+Vn+u/f:1-Vn-u/f;if(n<28){if(o=1/(n*n),n<2.857142857142857)i=Un+o*function(r){return 0===r?-.6938585727071818:r*(r*(r*(r*(r*(-9.814329344169145*r-81.2874355063066)-184.60509290671104)-162.39666946257347)-62.375332450326006)-10.558626225323291)-.6938585727071818}(o),o=In+o*function(r){return 0===r?19.651271667439257:19.651271667439257+r*(137.65775414351904+r*(434.56587747522923+r*(645.3872717332679+r*(429.00814002756783+r*(108.63500554177944+r*(6.570249770319282+-.0604244152148581*r))))))}(o);else{if(r<-6)return 2-_n;i=Mn+o*function(r){return 0===r?-.799283237680523:r*(r*(r*(r*(-483.5191916086514*r-1025.0951316110772)-637.5664433683896)-160.63638485582192)-17.757954917754752)-.799283237680523}(o),o=zn+o*function(r){return 0===r?30.33806074348246:30.33806074348246+r*(325.7925129965739+r*(1536.729586084437+r*(3199.8582195085955+r*(2553.0504064331644+r*(474.52854120695537+-22.44095244658582*r)))))}(o)}return i=xn(-(t=Nn(n,0))*t-.5625)*xn((t-n)*(t+n)+i/o),e?2-i/n:i/n}return e?2-_n:_n*_n}var Wn=Math.sqrt;function Cn(r){return function(){return r}}function Gn(r){return Jr(r)?Cn(NaN):function(e){return Jr(e)?NaN:e<r?0:1}}function $n(r,e){var n;return Jr(r)||Jr(e)||e<0?Cn(NaN):0===e?Gn(r):(n=e*Wn(2),function(e){return Jr(e)?NaN:.5*Rn(-(e-r)/n)})}function Hn(r){return R(r)===r&&r>0}function Bn(r){return r==r&&r>ur&&r<ar}z((function(r,e){return Jr(r)||Jr(e)?NaN:r<e?0:1}),"factory",Gn),z((function(r,e,n){return Jr(r)||Jr(e)||Jr(n)||n<0?NaN:0===n?r<e?0:1:.5*Rn(-(r-e)/(n*Wn(2)))}),"factory",$n);var Zn,Xn=Math.round,Yn=.6931471805599453;function qn(r){return r}Zn=function(r,e){var n,t;if(!zr(r))throw new TypeError(S("invalid argument. First argument must be a function. Value: `%s`.",r));if(arguments.length<2)n=qn;else if(!zr(n=e))throw new TypeError(S("invalid argument. Hash function argument must be a function. Value: `%s`.",n));return z(i,"cache",t={}),i;function i(){var e,i,o,a;for(e=new Array(arguments.length),a=0;a<arguments.length;a++)e[a]=arguments[a];return o=n(e).toString(),Y(t,o)?t[o]:(i=r.apply(null,e),t[o]=i,i)}}((function(r,e){var n;return 0===e?0===r?1:0:(n=e*(e+1)/2,r<0||r>n?0:(r>n/2&&(r=n-r),Zn(r-e,e-1)+Zn(r,e-1)))}));var Dn=Zn;function Jn(r,e){var n,t,i;if(Jr(r)||!Hn(e)||!Bn(e))return NaN;if(r<0)return 0;if((r=Xn(r))>=e*(e+1)/2)return 1;for(n=xn(-e*Yn),i=0,t=0;t<=r;t++)i+=Dn(t,e)*n;return i}function Kn(r,e,n){var t,i;if(!Br(r)&&!Zr(r))throw new TypeError(S("invalid argument. First argument must be an array-like object. Value: `%s`.",r));if(0===(t=r.length))return-1;if(3===arguments.length){if(!cr(n))throw new TypeError(S("invalid argument. Third argument must be an integer. Value: `%s`.",n));if(n>=0){if(n>=t)return-1;i=n}else(i=t+n)<0&&(i=0)}else i=0;if(re(e)){for(;i<t;i++)if(re(r[i]))return i}else for(;i<t;i++)if(r[i]===e)return i;return-1}function Qn(){var r,e=arguments,n="https://stdlib.io/e/"+e[0]+"?";for(r=1;r<e.length;r++)n+="&arg[]="+encodeURIComponent(e[r]);return n}z(Jn,"factory",(function(r){var e,n;return Hn(r)&&Bn(r)?(n=xn(-r*Yn),e=r*(r+1)/2,function(t){var i,o;if(Jr(t))return NaN;if(t<0)return 0;if((t=Xn(t))>=e)return 1;for(o=0,i=0;i<=t;i++)o+=Dn(i,r)*n;return o}):Cn(NaN)}));var rt=["two-sided","less","greater"],et=["pratt","wilcox","zsplit"];function nt(r,e){return r-e}function tt(r){return cr(r)&&r>0}function it(r){return lr(r)&&r.valueOf()>0}function ot(r){return tt(r)||it(r)}function at(r){return W(r/2)}function ut(r){return at(r>0?r-1:r+1)}function ft(r){return 0|r}z(ot,"isPrimitive",tt),z(ot,"isObject",it);var ct=1072693247,lt=1e300,st=1e-300,pt=!0===Me?1:0,ht=new Ae(1),gt=new be(ht.buffer);function yt(r,e){return ht[0]=r,gt[pt]=e>>>0,ht[0]}var dt=1048575,vt=1048576,wt=1072693248,bt=536870912,mt=524288,Et=20,jt=9007199254740992,xt=.9617966939259756,At=.9617967009544373,Tt=-7.028461650952758e-9,Ot=[1,1.5],Nt=[0,.5849624872207642],_t=[0,1.350039202129749e-8],St=1.4426950408889634,Vt=1.4426950216293335,kt=1.9259629911266175e-8,Lt=1048575,Ft=1048576,Pt=1071644672,Ut=20,It=.6931471824645996,Mt=-1.904654299957768e-9,zt=1072693247,Rt=1105199104,Wt=1139802112,Ct=1083179008,Gt=1072693248,$t=1083231232,Ht=3230714880,Bt=31,Zt=1e300,Xt=1e-300,Yt=8008566259537294e-32,qt=[0,0],Dt=[0,0];function Jt(r,e){var n,t,i,o,a,u,f,c,l,s,p,h,g,y;if(Jr(r)||Jr(e))return NaN;if(He.assign(e,qt,1,0),a=qt[0],0===qt[1]){if(0===e)return 1;if(1===e)return r;if(-1===e)return 1/r;if(.5===e)return Wn(r);if(-.5===e)return 1/Wn(r);if(2===e)return r*r;if(3===e)return r*r*r;if(4===e)return(r*=r)*r;if(se(e))return function(r,e){return-1===r?(r-r)/(r-r):1===r?1:un(r)<1==(e===ar)?0:ar}(r,e)}if(He.assign(r,qt,1,0),o=qt[0],0===qt[1]){if(0===o)return function(r,e){return e===ur?ar:e===ar?0:e>0?ut(e)?r:0:ut(e)?on(ar,r):ar}(r,e);if(1===r)return 1;if(-1===r&&ut(e))return-1;if(se(r))return r===ur?Jt(-0,-e):e<0?0:ar}if(r<0&&!1===W(e))return(r-r)/(r-r);if(i=un(r),n=o&ge|0,t=a&ge|0,f=a>>>Bt|0,u=(u=o>>>Bt|0)&&ut(e)?-1:1,t>Rt){if(t>Wt)return function(r,e){return(De(r)&ge)<=ct?e<0?lt*lt:st*st:e>0?lt*lt:st*st}(r,e);if(n<zt)return 1===f?u*Zt*Zt:u*Xt*Xt;if(n>Gt)return 0===f?u*Zt*Zt:u*Xt*Xt;p=function(r,e){var n,t,i,o,a,u,f;return o=(i=e-1)*i*(0===(f=i)?.5:.5+f*(.25*f-.3333333333333333)),n=(u=i*kt-o*St)-((t=Nn(t=(a=Vt*i)+u,0))-a),r[0]=t,r[1]=n,r}(Dt,i)}else p=function(r,e,n){var t,i,o,a,u,f,c,l,s,p,h,g,y,d,v,w,b,m,E,j,x;return m=0,n<vt&&(m-=53,n=De(e*=jt)),m+=(n>>Et)-ue|0,n=(E=n&dt|0)|wt|0,E<=235662?j=0:E<767610?j=1:(j=0,m+=1,n-=vt),a=Nn(i=(w=(e=yt(e,n))-(c=Ot[j]))*(b=1/(e+c)),0),t=(n>>1|bt)+mt,f=yt(0,t+=j<<18),v=(o=i*i)*o*(0===(x=o)?.5999999999999946:.5999999999999946+x*(.4285714285785502+x*(.33333332981837743+x*(.272728123808534+x*(.23066074577556175+.20697501780033842*x))))),f=Nn(f=3+(o=a*a)+(v+=(u=b*(w-a*f-a*(e-(f-c))))*(a+i)),0),s=Nn(s=(w=a*f)+(b=u*f+(v-(f-3-o))*i),0),p=At*s,y=(h=Tt*s+(b-(s-w))*xt+_t[j])-((g=Nn(g=p+h+(l=Nt[j])+(d=m),0))-d-l-p),r[0]=g,r[1]=y,r}(Dt,i,n);if(h=(s=(e-(c=Nn(e,0)))*p[0]+e*p[1])+(l=c*p[0]),He.assign(h,qt,1,0),g=ft(qt[0]),y=ft(qt[1]),g>=Ct){if(0!=(g-Ct|y))return u*Zt*Zt;if(s+Yt>h-l)return u*Zt*Zt}else if((g&ge)>=$t){if(0!=(g-Ht|y))return u*Xt*Xt;if(s<=h-l)return u*Xt*Xt}return h=function(r,e,n){var t,i,o,a,u,f,c,l,s,p;return s=((l=r&ge|0)>>Ut)-ue|0,c=0,l>Pt&&(i=yt(0,((c=r+(Ft>>s+1)>>>0)&~(Lt>>(s=((c&ge)>>Ut)-ue|0)))>>>0),c=(c&Lt|Ft)>>Ut-s>>>0,r<0&&(c=-c),e-=i),r=ft(r=De(f=1-((f=(o=(i=Nn(i=n+e,0))*It)+(a=(n-(i-e))*Yn+i*Mt))*(t=f-(i=f*f)*(0===(p=i)?.16666666666666602:.16666666666666602+p*(p*(6613756321437934e-20+p*(4.1381367970572385e-8*p-16533902205465252e-22))-.0027777777777015593)))/(t-2)-((u=a-(f-o))+f*u)-f))),(r+=c<<Ut>>>0)>>Ut<=0?yn(f,c):yt(f,r)}(g,l,s),u*h}var Kt=308,Qt=-308,ri=-324,ei=9007199254740992,ni=1e308;function ti(r,e){var n,t;return Jr(r)||Jr(e)||se(e)?NaN:se(r)||0===r||e<ri||un(r)>ei&&e<=0?r:e>Kt?0*r:e<Qt?(n=Jt(10,-(e+Kt)),se(t=r*ni*n)?r:Xn(t)/ni/n):se(t=r*(n=Jt(10,-e)))?r:Xn(t)/n}function ii(r){var e,n,t;if(n=4,e=!0,arguments.length>0){if(!Hr(r))throw new TypeError(Qn("1Lx3L",r));if(Y(r,"digits")){if(!ot(r.digits))throw new TypeError(Qn("1Lx3P","digits",r.digits));n=r.digits}if(Y(r,"decision")){if(!Er(r.decision))throw new TypeError(Qn("1Lx2o","decision",r.decision));e=r.decision}}switch(t="",t+=this.method,t+="\n\n",t+="Alternative hypothesis: ","Paired Wilcoxon signed rank test"===this.method?t+="Median of the difference `x - y` is ":t+="Median of `x` is ",this.alternative){case"less":t+="less than ";break;case"greater":t+="greater than ";break;default:t+="not equal to "}return t+=this.nullValue,t+="\n\n",t+="    pValue: "+ti(this.pValue,-n)+"\n",t+="    statistic: "+ti(this.statistic,-n)+"\n",t+="\n",e&&(t+="Test Decision: ",this.rejected?t+="Reject null in favor of alternative at "+100*this.alpha+"% significance level":t+="Fail to reject null in favor of alternative at "+100*this.alpha+"% significance level",t+="\n"),t}var oi=$n(0,1);return function(){var r,e,n,t,i,o,a,u,f,c,l,s,p,h,g,y,d,v,w,b,m,E,j,x,A,T,O,N,_;if(!dr(N=arguments[0])&&!tr(N))throw new TypeError(Qn("1Lx8R",N));if(y=N.length,arguments.length>1)if(Hr(arguments[1]))n=arguments[1];else{if(!dr(_=arguments[1])&&!tr(_))throw new TypeError(Qn("1LxA5","y",_));if(y!==_.length)throw new Error(Qn("1Lx1E"));arguments.length>2&&(n=arguments[2])}if(s={},n&&(g=function(r,e){if(!Hr(e))return new TypeError(Qn("1Lx2V",e));if(Y(e,"alpha")){if(r.alpha=e.alpha,!$(r.alpha)||re(r.alpha))return new TypeError(Qn("1Lx8P","alpha",r.alpha));if(r.alpha<0||r.alpha>1)return new RangeError(Qn("1Lx8V","alpha",r.alpha))}if(Y(e,"alternative")){if(r.alternative=e.alternative,!Zr(r.alternative))return new TypeError(Qn("1Lx2W","alternative",r.alternative));if(!ee(rt,r.alternative))return new Error(Qn("1Lx4S","alternative",rt.join('", "'),r.alternative))}if(Y(e,"correction")&&(r.correction=e.correction,!Er(r.correction)||re(r.correction)))return new TypeError(Qn("1Lx2o","correction",r.alpha));if(Y(e,"exact")&&(r.exact=e.exact,!Er(r.exact)||re(r.exact)))return new TypeError(Qn("1Lx2o","exact",r.alpha));if(Y(e,"mu")&&(r.mu=e.mu,!$(r.mu)||re(r.mu)))return new TypeError(Qn("1Lx8P","mu",r.mu));if(Y(e,"zeroMethod")){if(r.zeroMethod=e.zeroMethod,!Zr(r.zeroMethod))return new TypeError(Qn("1Lx2W","zeroMethod",r.alternative));if(!ee(et,r.zeroMethod))return new Error(Qn("1Lx4S","zeroMethod",et.join('", "'),r.zeroMethod))}return null}(s,n)))throw g;if(b=s.mu||0,r=void 0===s.correction||s.correction,c=void 0===s.alpha?.05:s.alpha,y<2)throw new Error(Qn("1LxA2",N));if(h=s.alternative||"two-sided","wilcox"===(e=s.zeroMethod||"wilcox")){if(j=[],_)for(x=0;x<y;x++)0!=(O=N[x]-_[x]-b)&&j.push(O);else for(x=0;x<y;x++)0!==N[x]&&j.push(N[x]-b);u=N.length-j.length}else if(j=new Ae(y),u=0,_)for(x=0;x<y;x++)j[x]=N[x]-_[x]-b,0===j[x]&&(u+=1);else for(x=0;x<y;x++)j[x]=N[x]-b,0===j[x]&&(u+=1);if(u===y)throw new Error(Qn("1Lx1J"));for(y=j.length,w=new Ae(y),x=0;x<y;x++)w[x]=un(j[x]);for(A=ie(w),a=0,f=0,x=0;x<y;x++)j[x]>0?a+=A[x]:0===j[x]&&(f+=A[x]);if(t=function(r){var e,n,t,i;for((r=r.slice()).sort(nt),e=r.length,t=1,i=0;t<e;t++)n=r[t],r[i]!==n&&(r[i+=1]=n);return r.length=i+1,r}(A).length!==A.length,"zsplit"===e&&(a+=f/2),T=a,m=y*(y+1)*.25,E=y*(y+1)*(2*y+1),"pratt"===e){for(d=[],x=0;x<y;x++)0!==j[x]&&d.push(A[x]);A=d,m-=u*(u+1)*.25,E-=u*(u+1)*(2*u+1)}for(i=function(r){var e,n,t,i,o,a,u;if(!Br(r))throw new TypeError(S("invalid argument. First argument must be a collection. Value: `%s`.",r));for(e=0,n=[],i=[],t=r.length,a=0;a<t;a++)e+=1,-1===(u=Kn(n,o=r[a]))?(n.push(o),i.push([o,1,0])):i[u][1]+=1;for(t=i.length,a=0;a<t;a++)i[a][2]=i[a][1]/e;return i}(A),o=0,x=0;x<i.length;x++)i[x][1]>1&&(o+=(O=i[x][1])*(O*O-1));if(o>0&&(E-=.5*o),E=Wn(E/24),y>50&&!s.exact||u>0||t){if(j=0,r)switch(h){case"two-sided":j=.5*function(r){return 0===r||Jr(r)?r:r<0?-1:1}(T-m);break;case"less":j=-.5;break;default:j=.5}p=(T-m-j)/E,l="two-sided"===h?2*(1-oi(un(p))):"greater"===h?1-oi(p):oi(p)}else p=T,l="two-sided"===h?p>y*(y+1)/4?2*(1-Jn(p-1,y)):2*Jn(p,y):"greater"===h?1-Jn(p-1,y):Jn(p,y);return vr(v={},"rejected",l<=c),vr(v,"alpha",c),vr(v,"pValue",l),vr(v,"statistic",T),vr(v,"nullValue",b),vr(v,"alternative",h),vr(v,"method",(_?"Paired":"One-Sample")+" Wilcoxon signed rank test"),vr(v,"print",ii),v}},"object"==typeof exports&&"undefined"!=typeof module?module.exports=e():"function"==typeof define&&define.amd?define(e):(r="undefined"!=typeof globalThis?globalThis:r||self).wilcoxon=e();
//# sourceMappingURL=browser.js.map

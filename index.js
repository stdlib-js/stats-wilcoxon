// Copyright (c) 2024 The Stdlib Authors. License is Apache-2.0: http://www.apache.org/licenses/LICENSE-2.0
!function(r,e){"object"==typeof exports&&"undefined"!=typeof module?module.exports=e():"function"==typeof define&&define.amd?define(e):(r="undefined"!=typeof globalThis?globalThis:r||self).wilcoxon=e()}(this,(function(){"use strict";var r="function"==typeof Object.defineProperty?Object.defineProperty:null;var e=Object.defineProperty;function n(r){return"number"==typeof r}function t(r){var e,n="";for(e=0;e<r;e++)n+="0";return n}function i(r,e,n){var i=!1,o=e-r.length;return o<0||(function(r){return"-"===r[0]}(r)&&(i=!0,r=r.substr(1)),r=n?r+t(o):t(o)+r,i&&(r="-"+r)),r}var o=String.prototype.toLowerCase,a=String.prototype.toUpperCase;function u(r){var e,t,u;switch(r.specifier){case"b":e=2;break;case"o":e=8;break;case"x":case"X":e=16;break;default:e=10}if(t=r.arg,u=parseInt(t,10),!isFinite(u)){if(!n(t))throw new Error("invalid integer. Value: "+t);u=0}return u<0&&("u"===r.specifier||10!==e)&&(u=4294967295+u+1),u<0?(t=(-u).toString(e),r.precision&&(t=i(t,r.precision,r.padRight)),t="-"+t):(t=u.toString(e),u||r.precision?r.precision&&(t=i(t,r.precision,r.padRight)):t="",r.sign&&(t=r.sign+t)),16===e&&(r.alternate&&(t="0x"+t),t=r.specifier===a.call(r.specifier)?a.call(t):o.call(t)),8===e&&r.alternate&&"0"!==t.charAt(0)&&(t="0"+t),t}function f(r){return"string"==typeof r}var c=Math.abs,l=String.prototype.toLowerCase,s=String.prototype.toUpperCase,p=String.prototype.replace,g=/e\+(\d)$/,h=/e-(\d)$/,v=/^(\d+)$/,d=/^(\d+)e/,y=/\.0$/,m=/\.0*e/,b=/(\..*[^0])0*e/;function w(r){var e,t,i=parseFloat(r.arg);if(!isFinite(i)){if(!n(r.arg))throw new Error("invalid floating-point number. Value: "+t);i=r.arg}switch(r.specifier){case"e":case"E":t=i.toExponential(r.precision);break;case"f":case"F":t=i.toFixed(r.precision);break;case"g":case"G":c(i)<1e-4?((e=r.precision)>0&&(e-=1),t=i.toExponential(e)):t=i.toPrecision(r.precision),r.alternate||(t=p.call(t,b,"$1e"),t=p.call(t,m,"e"),t=p.call(t,y,""));break;default:throw new Error("invalid double notation. Value: "+r.specifier)}return t=p.call(t,g,"e+0$1"),t=p.call(t,h,"e-0$1"),r.alternate&&(t=p.call(t,v,"$1."),t=p.call(t,d,"$1.e")),i>=0&&r.sign&&(t=r.sign+t),t=r.specifier===s.call(r.specifier)?s.call(t):l.call(t)}function E(r){var e,n="";for(e=0;e<r;e++)n+=" ";return n}function j(r,e,n){var t=e-r.length;return t<0?r:r=n?r+E(t):E(t)+r}var O=String.fromCharCode,A=isNaN,T=Array.isArray;function x(r){var e={};return e.specifier=r.specifier,e.precision=void 0===r.precision?1:r.precision,e.width=r.width,e.flags=r.flags||"",e.mapping=r.mapping,e}function N(r){var e,n,t,o,a,c,l,s,p;if(!T(r))throw new TypeError("invalid argument. First argument must be an array. Value: `"+r+"`.");for(c="",l=1,s=0;s<r.length;s++)if(f(t=r[s]))c+=t;else{if(e=void 0!==t.precision,!(t=x(t)).specifier)throw new TypeError("invalid argument. Token is missing `specifier` property. Index: `"+s+"`. Value: `"+t+"`.");for(t.mapping&&(l=t.mapping),n=t.flags,p=0;p<n.length;p++)switch(o=n.charAt(p)){case" ":t.sign=" ";break;case"+":t.sign="+";break;case"-":t.padRight=!0,t.padZeros=!1;break;case"0":t.padZeros=n.indexOf("-")<0;break;case"#":t.alternate=!0;break;default:throw new Error("invalid flag: "+o)}if("*"===t.width){if(t.width=parseInt(arguments[l],10),l+=1,A(t.width))throw new TypeError("the argument for * width at position "+l+" is not a number. Value: `"+t.width+"`.");t.width<0&&(t.padRight=!0,t.width=-t.width)}if(e&&"*"===t.precision){if(t.precision=parseInt(arguments[l],10),l+=1,A(t.precision))throw new TypeError("the argument for * precision at position "+l+" is not a number. Value: `"+t.precision+"`.");t.precision<0&&(t.precision=1,e=!1)}switch(t.arg=arguments[l],t.specifier){case"b":case"o":case"x":case"X":case"d":case"i":case"u":e&&(t.padZeros=!1),t.arg=u(t);break;case"s":t.maxWidth=e?t.precision:-1;break;case"c":if(!A(t.arg)){if((a=parseInt(t.arg,10))<0||a>127)throw new Error("invalid character code. Value: "+t.arg);t.arg=A(a)?String(t.arg):O(a)}break;case"e":case"E":case"f":case"F":case"g":case"G":e||(t.precision=6),t.arg=w(t);break;default:throw new Error("invalid specifier: "+t.specifier)}t.maxWidth>=0&&t.arg.length>t.maxWidth&&(t.arg=t.arg.substring(0,t.maxWidth)),t.padZeros?t.arg=i(t.arg,t.width||t.precision,t.padRight):t.width&&(t.arg=j(t.arg,t.width,t.padRight)),c+=t.arg||"",l+=1}return c}var _=/%(?:([1-9]\d*)\$)?([0 +\-#]*)(\*|\d+)?(?:(\.)(\*|\d+)?)?[hlL]?([%A-Za-z])/g;function V(r){var e={mapping:r[1]?parseInt(r[1],10):void 0,flags:r[2],width:r[3],precision:r[5],specifier:r[6]};return"."===r[4]&&void 0===r[5]&&(e.precision="1"),e}function k(r){var e,n,t,i;for(n=[],i=0,t=_.exec(r);t;)(e=r.slice(i,_.lastIndex-t[0].length)).length&&n.push(e),n.push(V(t)),i=_.lastIndex,t=_.exec(r);return(e=r.slice(i)).length&&n.push(e),n}function F(r){return"string"==typeof r}function S(r){var e,n,t;if(!F(r))throw new TypeError(S("invalid argument. First argument must be a string. Value: `%s`.",r));for(e=k(r),(n=new Array(arguments.length))[0]=e,t=1;t<n.length;t++)n[t]=arguments[t];return N.apply(null,n)}var U,P=Object.prototype,I=P.toString,M=P.__defineGetter__,z=P.__defineSetter__,L=P.__lookupGetter__,G=P.__lookupSetter__;U=function(){try{return r({},"x",{}),!0}catch(r){return!1}}()?e:function(r,e,n){var t,i,o,a;if("object"!=typeof r||null===r||"[object Array]"===I.call(r))throw new TypeError(S("invalid argument. First argument must be an object. Value: `%s`.",r));if("object"!=typeof n||null===n||"[object Array]"===I.call(n))throw new TypeError(S("invalid argument. Property descriptor must be an object. Value: `%s`.",n));if((i="value"in n)&&(L.call(r,e)||G.call(r,e)?(t=r.__proto__,r.__proto__=P,delete r[e],r[e]=n.value,r.__proto__=t):r[e]=n.value),o="get"in n,a="set"in n,i&&(o||a))throw new Error("invalid argument. Cannot specify one or more accessors and a value or writable attribute in the property descriptor.");return o&&M&&M.call(r,e,n.get),a&&z&&z.call(r,e,n.set),r};var R=U;function W(r,e,n){R(r,e,{configurable:!1,enumerable:!1,writable:!1,value:n})}var $=Math.floor;function C(r){return $(r)===r}function H(r){if("function"!=typeof r)throw new TypeError(S("invalid argument. Must provide a function. Value: `%s`.",r));return function(e){var n,t;if(!function(r){return null!=r&&"function"!=typeof r&&"number"==typeof r.length&&C(r.length)&&r.length>=0&&r.length<=4294967295}(e))return!1;if(0===(n=e.length))return!1;for(t=0;t<n;t++)if(!1===r(e[t]))return!1;return!0}}function B(r){return"number"==typeof r}var Z="function"==typeof Symbol&&"symbol"==typeof Symbol("foo");function X(){return Z&&"symbol"==typeof Symbol.toStringTag}var Y=Object.prototype.toString;var q=Object.prototype.hasOwnProperty;function D(r,e){return null!=r&&q.call(r,e)}var J="function"==typeof Symbol?Symbol:void 0,K="function"==typeof J?J.toStringTag:"";var Q=X()?function(r){var e,n,t;if(null==r)return Y.call(r);n=r[K],e=D(r,K);try{r[K]=void 0}catch(e){return Y.call(r)}return t=Y.call(r),e?r[K]=n:delete r[K],t}:function(r){return Y.call(r)},rr=Number,er=rr.prototype.toString;var nr=X();function tr(r){return"object"==typeof r&&(r instanceof rr||(nr?function(r){try{return er.call(r),!0}catch(r){return!1}}(r):"[object Number]"===Q(r)))}function ir(r){return B(r)||tr(r)}W(ir,"isPrimitive",B),W(ir,"isObject",tr);var or=H(ir.isPrimitive),ar=H(ir.isObject),ur=H(ir);W(ur,"primitives",or),W(ur,"objects",ar);var fr=Number.POSITIVE_INFINITY,cr=rr.NEGATIVE_INFINITY;function lr(r){return r<fr&&r>cr&&C(r)}function sr(r){return B(r)&&lr(r)}function pr(r){return tr(r)&&lr(r.valueOf())}function gr(r){return sr(r)||pr(r)}function hr(r){return sr(r)&&r>=0}function vr(r){return pr(r)&&r.valueOf()>=0}function dr(r){return hr(r)||vr(r)}W(gr,"isPrimitive",sr),W(gr,"isObject",pr),W(dr,"isPrimitive",hr),W(dr,"isObject",vr);var yr=9007199254740991;function mr(r){return null!==r&&"object"==typeof r&&hr(r.length)&&r.length<=yr&&"number"==typeof r.BYTES_PER_ELEMENT&&"number"==typeof r.byteOffset&&"number"==typeof r.byteLength}function br(r,e,n){R(r,e,{configurable:!1,enumerable:!0,writable:!1,value:n})}var wr=Array.isArray?Array.isArray:function(r){return"[object Array]"===Q(r)};function Er(r){return"object"==typeof r&&null!==r&&!wr(r)}var jr=/./;function Or(r){return"boolean"==typeof r}var Ar=Boolean,Tr=Boolean.prototype.toString;var xr=X();function Nr(r){return"object"==typeof r&&(r instanceof Ar||(xr?function(r){try{return Tr.call(r),!0}catch(r){return!1}}(r):"[object Boolean]"===Q(r)))}function _r(r){return Or(r)||Nr(r)}function Vr(){return new Function("return this;")()}W(_r,"isPrimitive",Or),W(_r,"isObject",Nr);var kr="object"==typeof self?self:null,Fr="object"==typeof window?window:null,Sr="object"==typeof global?global:null,Ur="object"==typeof globalThis?globalThis:null;var Pr=function(r){if(arguments.length){if(!Or(r))throw new TypeError(S("invalid argument. Must provide a boolean. Value: `%s`.",r));if(r)return Vr()}if(Ur)return Ur;if(kr)return kr;if(Fr)return Fr;if(Sr)return Sr;throw new Error("unexpected error. Unable to resolve global object.")}(),Ir=Pr.document&&Pr.document.childNodes,Mr=Int8Array;function zr(){return/^\s*function\s*([^(]*)/i}var Lr=/^\s*function\s*([^(]*)/i;function Gr(r){return null!==r&&"object"==typeof r}function Rr(r){var e,n,t,i;if(("Object"===(n=Q(r).slice(8,-1))||"Error"===n)&&r.constructor){if("string"==typeof(t=r.constructor).name)return t.name;if(e=Lr.exec(t.toString()))return e[1]}return Gr(i=r)&&(i._isBuffer||i.constructor&&"function"==typeof i.constructor.isBuffer&&i.constructor.isBuffer(i))?"Buffer":n}W(zr,"REGEXP",Lr),W(Gr,"isObjectLikeArray",function(r){if("function"!=typeof r)throw new TypeError(S("invalid argument. Must provide a function. Value: `%s`.",r));return function(e){var n,t;if(!wr(e))return!1;if(0===(n=e.length))return!1;for(t=0;t<n;t++)if(!1===r(e[t]))return!1;return!0}}(Gr));var Wr="function"==typeof jr||"object"==typeof Mr||"function"==typeof Ir?function(r){return Rr(r).toLowerCase()}:function(r){var e;return null===r?"null":"object"===(e=typeof r)?Rr(r).toLowerCase():e};function $r(r){return"function"===Wr(r)}var Cr,Hr=Object,Br=Object.getPrototypeOf;Cr=$r(Object.getPrototypeOf)?Br:function(r){var e=function(r){return r.__proto__}(r);return e||null===e?e:"[object Function]"===Q(r.constructor)?r.constructor.prototype:r instanceof Object?Object.prototype:null};var Zr=Cr;var Xr=Object.prototype;function Yr(r){var e;return!!Er(r)&&(e=function(r){return null==r?null:(r=Hr(r),Zr(r))}(r),!e||!D(r,"constructor")&&D(e,"constructor")&&$r(e.constructor)&&"[object Function]"===Q(e.constructor)&&D(e,"isPrototypeOf")&&$r(e.isPrototypeOf)&&(e===Xr||function(r){var e;for(e in r)if(!D(r,e))return!1;return!0}(r)))}function qr(r){return"object"==typeof r&&null!==r&&"number"==typeof r.length&&C(r.length)&&r.length>=0&&r.length<=yr}function Dr(r){return"string"==typeof r}var Jr=String.prototype.valueOf;var Kr=X();function Qr(r){return"object"==typeof r&&(r instanceof String||(Kr?function(r){try{return Jr.call(r),!0}catch(r){return!1}}(r):"[object String]"===Q(r)))}function re(r){return Dr(r)||Qr(r)}function ee(r){return r!=r}function ne(r){return B(r)&&ee(r)}function te(r){return tr(r)&&ee(r.valueOf())}function ie(r){return ne(r)||te(r)}function oe(r,e,n){var t,i,o;if(!qr(r)&&!Dr(r))throw new TypeError(S("invalid argument. First argument must be array-like. Value: `%s`.",r));if(arguments.length<2)throw new Error("insufficient arguments. Must provide a search value.");if(arguments.length>2){if(!sr(n))throw new TypeError(S("invalid argument. Third argument must be an integer. Value: `%s`.",n));(i=n)<0&&(i=0)}else i=0;if(Dr(r)){if(!Dr(e))throw new TypeError(S("invalid argument. Second argument must be a string. Value: `%s`.",e));return-1!==r.indexOf(e,i)}if(t=r.length,ne(e)){for(o=i;o<t;o++)if(ne(r[o]))return!0;return!1}for(o=i;o<t;o++)if(r[o]===e)return!0;return!1}function ae(r){var e,n,t;for(e=r.length,n=0,t=0;t<e;t++)n+=r[t];return n}function ue(r){var e,n;for(e=new Array(r.length),n=0;n<r.length;n++)e[n]=n;return e.sort((function(e,n){return function(r,e){return r<e?-1:r>e?1:0}(r[e],r[n])}))}function fe(r,e){var n,t,i;for(n=r.length,t=new Array(n),i=0;i<n;i++)t[i]=oe(e,r[i]);return t}W(re,"isPrimitive",Dr),W(re,"isObject",Qr),W(ie,"isPrimitive",ne),W(ie,"isObject",te);var ce=["min","max","average","dense","ordinal"],le=["last","first","remove"];function se(r,e){return Er(e)?D(e,"encoding")&&(r.encoding=e.encoding,!wr(r.encoding))?new TypeError(S("invalid option. `%s` option must be an array. Option: `%s`.","encoding",r.encoding)):!D(e,"method")||(r.method=e.method,Dr(r.method)&&oe(ce,r.method))?!D(e,"missing")||(r.missing=e.missing,Dr(r.missing)&&oe(le,r.missing))?null:new TypeError(S('invalid option. `%s` option must be one of the following: "%s". Option: `%s`.',"missing",le.join('", "'),r.missing)):new TypeError(S('invalid option. `%s` option must be one of the following: "%s". Option: `%s`.',"method",ce.join('", "'),r.method)):new TypeError(S("invalid argument. Options argument must be an object. Value: `%s`.",e))}function pe(r,e){var n,t,i,o,a,u,f,c,l,s,p,g,h,v,d,y,m,b;if(!qr(r))throw new TypeError(S("invalid argument. First argument must be an array-like object. Value: `%s`.",r));if(h={},arguments.length>1&&(d=se(h,e)))throw d;for(p=h.method||"average",u=h.encoding||[null,NaN],l=h.missing||"last",y=r.length,v=[],m=0;m<y;m++)oe(u,r[m])||v.push(r[m]);if(n=fe(r,u),y=v.length,o=0,g=new Array(y),c=ue(v),"ordinal"===p)for(m=0;m<y;m++)g[c[m]]=m+1;else for(t=0,m=0;m<y;m++)if(f=m+1,m===y-1||v[c[m]]!==v[c[f]]){switch(p){case"min":s=f-t;break;case"max":s=f;break;case"dense":s=f-t-o,o+=t;break;default:s=f-.5*t}for(b=m-t;b<f;b++)g[c[b]]=s;t=0}else t+=1;if("first"===l){for(i=ae(n),b=1,a=new Array(n.length),m=0;m<n.length;m++)n[m]?(a[m]=b,b+=1):a[m]=g.shift()+i;return a}if("last"===l){for(a=new Array(n.length),m=0;m<n.length;m++)n[m]?a[m]=m+g.length+1:a[m]=g.shift();return a}return g}var ge=Math.ceil;function he(r){return r<0?ge(r):$(r)}var ve=1023;function de(r){return r===fr||r===cr}var ye=2147483647,me="function"==typeof Uint32Array;var be="function"==typeof Uint32Array?Uint32Array:null;var we,Ee="function"==typeof Uint32Array?Uint32Array:void 0;we=function(){var r,e,n;if("function"!=typeof be)return!1;try{e=new be(e=[1,3.14,-3.14,4294967296,4294967297]),n=e,r=(me&&n instanceof Uint32Array||"[object Uint32Array]"===Q(n))&&1===e[0]&&3===e[1]&&4294967293===e[2]&&0===e[3]&&1===e[4]}catch(e){r=!1}return r}()?Ee:function(){throw new Error("not implemented")};var je=we,Oe="function"==typeof Float64Array;var Ae="function"==typeof Float64Array?Float64Array:null;var Te,xe="function"==typeof Float64Array?Float64Array:void 0;Te=function(){var r,e,n;if("function"!=typeof Ae)return!1;try{e=new Ae([1,3.14,-3.14,NaN]),n=e,r=(Oe&&n instanceof Float64Array||"[object Float64Array]"===Q(n))&&1===e[0]&&3.14===e[1]&&-3.14===e[2]&&e[3]!=e[3]}catch(e){r=!1}return r}()?xe:function(){throw new Error("not implemented")};var Ne=Te,_e="function"==typeof Uint8Array;var Ve="function"==typeof Uint8Array?Uint8Array:null;var ke,Fe="function"==typeof Uint8Array?Uint8Array:void 0;ke=function(){var r,e,n;if("function"!=typeof Ve)return!1;try{e=new Ve(e=[1,3.14,-3.14,256,257]),n=e,r=(_e&&n instanceof Uint8Array||"[object Uint8Array]"===Q(n))&&1===e[0]&&3===e[1]&&253===e[2]&&0===e[3]&&1===e[4]}catch(e){r=!1}return r}()?Fe:function(){throw new Error("not implemented")};var Se=ke,Ue="function"==typeof Uint16Array;var Pe="function"==typeof Uint16Array?Uint16Array:null;var Ie,Me="function"==typeof Uint16Array?Uint16Array:void 0;Ie=function(){var r,e,n;if("function"!=typeof Pe)return!1;try{e=new Pe(e=[1,3.14,-3.14,65536,65537]),n=e,r=(Ue&&n instanceof Uint16Array||"[object Uint16Array]"===Q(n))&&1===e[0]&&3===e[1]&&65533===e[2]&&0===e[3]&&1===e[4]}catch(e){r=!1}return r}()?Me:function(){throw new Error("not implemented")};var ze,Le={uint16:Ie,uint8:Se};(ze=new Le.uint16(1))[0]=4660;var Ge,Re,We=52===new Le.uint8(ze.buffer)[0];!0===We?(Ge=1,Re=0):(Ge=0,Re=1);var $e={HIGH:Ge,LOW:Re},Ce=new Ne(1),He=new je(Ce.buffer),Be=$e.HIGH,Ze=$e.LOW;function Xe(r,e,n,t){return Ce[0]=r,e[t]=He[Be],e[t+n]=He[Ze],e}function Ye(r){return Xe(r,[0,0],1,0)}W(Ye,"assign",Xe);var qe,De,Je=!0===We?1:0,Ke=new Ne(1),Qe=new je(Ke.buffer);function rn(r){return Ke[0]=r,Qe[Je]}!0===We?(qe=1,De=0):(qe=0,De=1);var en={HIGH:qe,LOW:De},nn=new Ne(1),tn=new je(nn.buffer),on=en.HIGH,an=en.LOW;function un(r,e){return tn[on]=r,tn[an]=e,nn[0]}var fn=[0,0];function cn(r,e){var n,t;return Ye.assign(r,fn,1,0),n=fn[0],n&=ye,t=rn(e),un(n|=t&=2147483648,fn[1])}function ln(r){return Math.abs(r)}function sn(r,e,n,t){return ee(r)||de(r)?(e[t]=r,e[t+n]=0,e):0!==r&&ln(r)<22250738585072014e-324?(e[t]=4503599627370496*r,e[t+n]=-52,e):(e[t]=r,e[t+n]=0,e)}W((function(r){return sn(r,[0,0],1,0)}),"assign",sn);var pn=[0,0],gn=[0,0];function hn(r,e){var n,t;return 0===e||0===r||ee(r)||de(r)?r:(sn(r,pn,1,0),e+=pn[1],e+=function(r){var e=rn(r);return(e=(2146435072&e)>>>20)-ve|0}(r=pn[0]),e<-1074?cn(0,r):e>1023?r<0?cr:fr:(e<=-1023?(e+=52,t=2220446049250313e-31):t=1,Ye.assign(r,gn,1,0),n=gn[0],n&=2148532223,t*un(n|=e+ve<<20,gn[1])))}var vn=1.4426950408889634,dn=1/(1<<28);function yn(r){var e;return ee(r)||r===fr?r:r===cr?0:r>709.782712893384?fr:r<-745.1332191019411?0:r>-3.725290298461914e-9&&r<dn?1+r:function(r,e,n){var t,i,o,a;return hn(1-(e-(t=r-e)*(o=t-(i=t*t)*(0===(a=i)?.16666666666666602:.16666666666666602+a*(a*(6613756321437934e-20+a*(4.1381367970572385e-8*a-16533902205465252e-22))-.0027777777777015593)))/(2-o)-r),n)}(r-.6931471803691238*(e=he(r<0?vn*r-.5:vn*r+.5)),1.9082149292705877e-10*e,e)}var mn=!0===We?0:1,bn=new Ne(1),wn=new je(bn.buffer);function En(r,e){return bn[0]=r,wn[mn]=e>>>0,bn[0]}var jn=.8450629115104675;function On(r){var e,n,t,i,o,a,u,f;if(ee(r))return NaN;if(r===fr)return 0;if(r===cr)return 2;if(0===r)return 1;if(r<0?(e=!0,n=-r):(e=!1,n=r),n<.84375)return n<13877787807814457e-33?1-r:(i=.12837916709551256+(t=r*r)*function(r){return 0===r?-.3250421072470015:r*(r*(-23763016656650163e-21*r-.005770270296489442)-.02848174957559851)-.3250421072470015}(t),o=1+t*function(r){return 0===r?.39791722395915535:.39791722395915535+r*(.0650222499887673+r*(.005081306281875766+r*(.00013249473800432164+-3960228278775368e-21*r)))}(t),a=i/o,r<.25?1-(r+r*a):(i=r*a,.5-(i+=r-.5)));if(n<1.25)return u=(o=n-1)*function(r){return 0===r?.41485611868374833:.41485611868374833+r*(r*(.31834661990116175+r*(r*(.035478304325618236+-.002166375594868791*r)-.11089469428239668))-.3722078760357013)}(o)-.0023621185607526594,f=1+o*function(r){return 0===r?.10642088040084423:.10642088040084423+r*(.540397917702171+r*(.07182865441419627+r*(.12617121980876164+r*(.01363708391202905+.011984499846799107*r))))}(o),e?1+jn+u/f:1-jn-u/f;if(n<28){if(o=1/(n*n),n<2.857142857142857)i=o*function(r){return 0===r?-.6938585727071818:r*(r*(r*(r*(r*(-9.814329344169145*r-81.2874355063066)-184.60509290671104)-162.39666946257347)-62.375332450326006)-10.558626225323291)-.6938585727071818}(o)-.009864944034847148,o=1+o*function(r){return 0===r?19.651271667439257:19.651271667439257+r*(137.65775414351904+r*(434.56587747522923+r*(645.3872717332679+r*(429.00814002756783+r*(108.63500554177944+r*(6.570249770319282+-.0604244152148581*r))))))}(o);else{if(r<-6)return 2;i=o*function(r){return 0===r?-.799283237680523:r*(r*(r*(r*(-483.5191916086514*r-1025.0951316110772)-637.5664433683896)-160.63638485582192)-17.757954917754752)-.799283237680523}(o)-.0098649429247001,o=1+o*function(r){return 0===r?30.33806074348246:30.33806074348246+r*(325.7925129965739+r*(1536.729586084437+r*(3199.8582195085955+r*(2553.0504064331644+r*(474.52854120695537+-22.44095244658582*r)))))}(o)}return i=yn(-(t=En(n,0))*t-.5625)*yn((t-n)*(t+n)+i/o),e?2-i/n:i/n}return e?2:0}var An=Math.sqrt;function Tn(r){return function(){return r}}function xn(r){return ee(r)?Tn(NaN):function(e){if(ee(e))return NaN;return e<r?0:1}}function Nn(r,e){var n;return ee(r)||ee(e)||e<0?Tn(NaN):0===e?xn(r):(n=e*An(2),function(e){if(ee(e))return NaN;return.5*On(-(e-r)/n)})}function _n(r){return $(r)===r&&r>0}function Vn(r){return r==r&&r>cr&&r<fr}W((function(r,e){return ee(r)||ee(e)?NaN:r<e?0:1}),"factory",xn),W((function(r,e,n){return ee(r)||ee(e)||ee(n)||n<0?NaN:0===n?r<e?0:1:.5*On(-(r-e)/(n*An(2)))}),"factory",Nn);var kn,Fn=Math.round,Sn=.6931471805599453;function Un(r){return r}kn=function(r,e){var n,t;if(!$r(r))throw new TypeError(S("invalid argument. First argument must be a function. Value: `%s`.",r));if(arguments.length<2)n=Un;else if(!$r(n=e))throw new TypeError(S("invalid argument. Hash function argument must be a function. Value: `%s`.",n));return W(i,"cache",t={}),i;function i(){var e,i,o,a;for(e=new Array(arguments.length),a=0;a<arguments.length;a++)e[a]=arguments[a];return o=n(e).toString(),D(t,o)?t[o]:(i=r.apply(null,e),t[o]=i,i)}}((function(r,e){var n;return 0===e?0===r?1:0:(n=e*(e+1)/2,r<0||r>n?0:(r>n/2&&(r=n-r),kn(r-e,e-1)+kn(r,e-1)))}));var Pn=kn;function In(r,e){var n,t,i;if(ee(r)||!_n(e)||!Vn(e))return NaN;if(r<0)return 0;if((r=Fn(r))>=e*(e+1)/2)return 1;for(n=yn(-e*Sn),i=0,t=0;t<=r;t++)i+=Pn(t,e)*n;return i}function Mn(r,e,n){var t,i;if(!qr(r)&&!Dr(r))throw new TypeError(S("invalid argument. First argument must be an array-like object. Value: `%s`.",r));if(0===(t=r.length))return-1;if(3===arguments.length){if(!sr(n))throw new TypeError(S("invalid argument. Third argument must be an integer. Value: `%s`.",n));if(n>=0){if(n>=t)return-1;i=n}else(i=t+n)<0&&(i=0)}else i=0;if(ie(e)){for(;i<t;i++)if(ie(r[i]))return i}else for(;i<t;i++)if(r[i]===e)return i;return-1}function zn(r){var e,n,t,i,o,a,u;if(!qr(r))throw new TypeError(S("invalid argument. First argument must be a collection. Value: `%s`.",r));for(e=0,n=[],i=[],t=r.length,a=0;a<t;a++)e+=1,-1===(u=Mn(n,o=r[a]))?(n.push(o),i.push([o,1,0])):i[u][1]+=1;for(t=i.length,a=0;a<t;a++)i[a][2]=i[a][1]/e;return i}function Ln(r){return 0===r||ee(r)?r:r<0?-1:1}W(In,"factory",(function(r){var e,n;return _n(r)&&Vn(r)?(n=yn(-r*Sn),e=r*(r+1)/2,function(t){var i,o;if(ee(t))return NaN;if(t<0)return 0;if((t=Fn(t))>=e)return 1;for(o=0,i=0;i<=t;i++)o+=Pn(i,r)*n;return o}):Tn(NaN)}));var Gn=["two-sided","less","greater"],Rn=["pratt","wilcox","zsplit"];function Wn(r,e){if(!Yr(e))return new TypeError(S("invalid argument. Options argument must be an object. Value: `%s`.",e));if(D(e,"alpha")){if(r.alpha=e.alpha,!B(r.alpha)||ie(r.alpha))return new TypeError(S("invalid option. `%s` option must be a number. Option: `%s`.","alpha",r.alpha));if(r.alpha<0||r.alpha>1)return new RangeError(S("invalid option. `%s` option must be a number on the interval: [0, 1]. Option: `%f`.","alpha",r.alpha))}if(D(e,"alternative")){if(r.alternative=e.alternative,!Dr(r.alternative))return new TypeError(S("invalid option. `%s` option must be a string. Option: `%s`.","alternative",r.alternative));if(!oe(Gn,r.alternative))return new Error(S('invalid option. `%s` option must be one of the following: "%s". Option: `%s`.',"alternative",Gn.join('", "'),r.alternative))}if(D(e,"correction")&&(r.correction=e.correction,!Or(r.correction)||ie(r.correction)))return new TypeError(S("invalid option. `%s` option must be a boolean. Option: `%s`.","correction",r.alpha));if(D(e,"exact")&&(r.exact=e.exact,!Or(r.exact)||ie(r.exact)))return new TypeError(S("invalid option. `%s` option must be a boolean. Option: `%s`.","exact",r.alpha));if(D(e,"mu")&&(r.mu=e.mu,!B(r.mu)||ie(r.mu)))return new TypeError(S("invalid option. `%s` option must be a number. Option: `%s`.","mu",r.mu));if(D(e,"zeroMethod")){if(r.zeroMethod=e.zeroMethod,!Dr(r.zeroMethod))return new TypeError(S("invalid option. `%s` option must be a string. Option: `%s`.","zeroMethod",r.alternative));if(!oe(Rn,r.zeroMethod))return new Error(S('invalid option. `%s` option must be one of the following: "%s". Option: `%s`.',"zeroMethod",Rn.join('", "'),r.zeroMethod))}return null}function $n(r,e){return r-e}function Cn(r){var e,n,t,i;for((r=r.slice()).sort($n),e=r.length,t=1,i=0;t<e;t++)n=r[t],r[i]!==n&&(r[i+=1]=n);return r.length=i+1,r}function Hn(r){return sr(r)&&r>0}function Bn(r){return pr(r)&&r.valueOf()>0}function Zn(r){return Hn(r)||Bn(r)}function Xn(r){return C(r/2)}function Yn(r){return Xn(r>0?r-1:r+1)}function qn(r){return 0|r}W(Zn,"isPrimitive",Hn),W(Zn,"isObject",Bn);var Dn=!0===We?1:0,Jn=new Ne(1),Kn=new je(Jn.buffer);function Qn(r,e){return Jn[0]=r,Kn[Dn]=e>>>0,Jn[0]}var rt=1048576,et=[1,1.5],nt=[0,.5849624872207642],tt=[0,1.350039202129749e-8];var it=1048575;var ot=1048576;var at=1083179008,ut=1e300,ft=1e-300,ct=[0,0],lt=[0,0];function st(r,e){var n,t,i,o,a,u,f,c,l,s,p,g,h,v;if(ee(r)||ee(e))return NaN;if(Ye.assign(e,ct,1,0),a=ct[0],0===ct[1]){if(0===e)return 1;if(1===e)return r;if(-1===e)return 1/r;if(.5===e)return An(r);if(-.5===e)return 1/An(r);if(2===e)return r*r;if(3===e)return r*r*r;if(4===e)return(r*=r)*r;if(de(e))return function(r,e){return-1===r?(r-r)/(r-r):1===r?1:ln(r)<1==(e===fr)?0:fr}(r,e)}if(Ye.assign(r,ct,1,0),o=ct[0],0===ct[1]){if(0===o)return function(r,e){return e===cr?fr:e===fr?0:e>0?Yn(e)?r:0:Yn(e)?cn(fr,r):fr}(r,e);if(1===r)return 1;if(-1===r&&Yn(e))return-1;if(de(r))return r===cr?st(-0,-e):e<0?0:fr}if(r<0&&!1===C(e))return(r-r)/(r-r);if(i=ln(r),n=o&ye|0,t=a&ye|0,f=a>>>31|0,u=(u=o>>>31|0)&&Yn(e)?-1:1,t>1105199104){if(t>1139802112)return function(r,e){return(rn(r)&ye)<=1072693247?e<0?1/0:0:e>0?1/0:0}(r,e);if(n<1072693247)return 1===f?u*ut*ut:u*ft*ft;if(n>1072693248)return 0===f?u*ut*ut:u*ft*ft;p=function(r,e){var n,t,i,o,a,u;return n=(a=1.9259629911266175e-8*(i=e-1)-i*i*(0===(u=i)?.5:.5+u*(.25*u-.3333333333333333))*1.4426950408889634)-((t=En(t=(o=1.4426950216293335*i)+a,0))-o),r[0]=t,r[1]=n,r}(lt,i)}else p=function(r,e,n){var t,i,o,a,u,f,c,l,s,p,g,h,v,d,y,m,b,w,E,j,O;return w=0,n<rt&&(w-=53,n=rn(e*=9007199254740992)),w+=(n>>20)-ve|0,n=1072693248|(E=1048575&n|0),E<=235662?j=0:E<767610?j=1:(j=0,w+=1,n-=rt),a=En(i=(m=(e=Qn(e,n))-(c=et[j]))*(b=1/(e+c)),0),t=524288+(n>>1|536870912),f=Qn(0,t+=j<<18),y=(o=i*i)*o*(0===(O=o)?.5999999999999946:.5999999999999946+O*(.4285714285785502+O*(.33333332981837743+O*(.272728123808534+O*(.23066074577556175+.20697501780033842*O))))),f=En(f=3+(o=a*a)+(y+=(u=b*(m-a*f-a*(e-(f-c))))*(a+i)),0),v=(g=-7.028461650952758e-9*(s=En(s=(m=a*f)+(b=u*f+(y-(f-3-o))*i),0))+.9617966939259756*(b-(s-m))+tt[j])-((h=En(h=(p=.9617967009544373*s)+g+(l=nt[j])+(d=w),0))-d-l-p),r[0]=h,r[1]=v,r}(lt,i,n);if(g=(s=(e-(c=En(e,0)))*p[0]+e*p[1])+(l=c*p[0]),Ye.assign(g,ct,1,0),h=qn(ct[0]),v=qn(ct[1]),h>=at){if(0!=(h-at|v))return u*ut*ut;if(s+8008566259537294e-32>g-l)return u*ut*ut}else if((h&ye)>=1083231232){if(0!=(h-3230714880|v))return u*ft*ft;if(s<=g-l)return u*ft*ft}return g=function(r,e,n){var t,i,o,a,u,f,c,l,s,p;return s=((l=r&ye|0)>>20)-ve|0,c=0,l>1071644672&&(i=Qn(0,((c=r+(ot>>s+1)>>>0)&~(it>>(s=((c&ye)>>20)-ve|0)))>>>0),c=(c&it|ot)>>20-s>>>0,r<0&&(c=-c),e-=i),r=qn(r=rn(f=1-((f=(o=.6931471824645996*(i=En(i=n+e,0)))+(a=(n-(i-e))*Sn+-1.904654299957768e-9*i))*(t=f-(i=f*f)*(0===(p=i)?.16666666666666602:.16666666666666602+p*(p*(6613756321437934e-20+p*(4.1381367970572385e-8*p-16533902205465252e-22))-.0027777777777015593)))/(t-2)-((u=a-(f-o))+f*u)-f))),(r+=c<<20>>>0)>>20<=0?hn(f,c):Qn(f,r)}(h,l,s),u*g}var pt=1e308;function gt(r,e){var n,t;return ee(r)||ee(e)||de(e)?NaN:de(r)||0===r||e<-324||ln(r)>9007199254740992&&e<=0?r:e>308?0*r:e<-308?(n=st(10,-(e+308)),de(t=r*pt*n)?r:Fn(t)/pt/n):de(t=r*(n=st(10,-e)))?r:Fn(t)/n}function ht(r){var e,n,t;if(n=4,e=!0,arguments.length>0){if(!Yr(r))throw new TypeError(S("invalid argument. First argument must be an object. Value: `%s`.",r));if(D(r,"digits")){if(!Zn(r.digits))throw new TypeError(S("invalid option. `%s` option must be a positive integer. Option: `%s`.","digits",r.digits));n=r.digits}if(D(r,"decision")){if(!Or(r.decision))throw new TypeError(S("invalid option. `%s` option must be a boolean. Option: `%s`.","decision",r.decision));e=r.decision}}switch(t="",t+=this.method,t+="\n\n",t+="Alternative hypothesis: ","Paired Wilcoxon signed rank test"===this.method?t+="Median of the difference `x - y` is ":t+="Median of `x` is ",this.alternative){case"less":t+="less than ";break;case"greater":t+="greater than ";break;default:t+="not equal to "}return t+=this.nullValue,t+="\n\n",t+="    pValue: "+gt(this.pValue,-n)+"\n",t+="    statistic: "+gt(this.statistic,-n)+"\n",t+="\n",e&&(t+="Test Decision: ",this.rejected?t+="Reject null in favor of alternative at "+100*this.alpha+"% significance level":t+="Fail to reject null in favor of alternative at "+100*this.alpha+"% significance level",t+="\n"),t}var vt=Nn(0,1);return function(){var r,e,n,t,i,o,a,u,f,c,l,s,p,g,h,v,d,y,m,b,w,E,j,O,A,T,x,N,_;if(!mr(N=arguments[0])&&!or(N))throw new TypeError(S("invalid argument. First argument must be a numeric array. Value: `%s`.",N));if(v=N.length,arguments.length>1)if(Yr(arguments[1]))n=arguments[1];else{if(!mr(_=arguments[1])&&!or(_))throw new TypeError(S("invalid argument. `%s` argument must be a numeric array. Value: `%s`.","y",_));if(v!==_.length)throw new Error("invalid arguments. First and second arguments must have the same length.");arguments.length>2&&(n=arguments[2])}if(s={},n&&(h=Wn(s,n)))throw h;if(b=s.mu||0,r=void 0===s.correction||s.correction,c=void 0===s.alpha?.05:s.alpha,v<2)throw new Error(S("invalid argument. First argument must contain at least two elements. Value: `%s`.",N));if(g=s.alternative||"two-sided","wilcox"===(e=s.zeroMethod||"wilcox")){if(j=[],_)for(O=0;O<v;O++)0!==(x=N[O]-_[O]-b)&&j.push(x);else for(O=0;O<v;O++)0!==N[O]&&j.push(N[O]-b);u=N.length-j.length}else if(j=new Ne(v),u=0,_)for(O=0;O<v;O++)j[O]=N[O]-_[O]-b,0===j[O]&&(u+=1);else for(O=0;O<v;O++)j[O]=N[O]-b,0===j[O]&&(u+=1);if(u===v)throw new Error("`x` or `x - y` cannot be zero for all elements.");for(v=j.length,m=new Ne(v),O=0;O<v;O++)m[O]=ln(j[O]);for(A=pe(m),a=0,f=0,O=0;O<v;O++)j[O]>0?a+=A[O]:0===j[O]&&(f+=A[O]);if(t=Cn(A).length!==A.length,"zsplit"===e&&(a+=f/2),T=a,w=v*(v+1)*.25,E=v*(v+1)*(2*v+1),"pratt"===e){for(d=[],O=0;O<v;O++)0!==j[O]&&d.push(A[O]);A=d,w-=u*(u+1)*.25,E-=u*(u+1)*(2*u+1)}for(i=zn(A),o=0,O=0;O<i.length;O++)i[O][1]>1&&(o+=(x=i[O][1])*(x*x-1));if(o>0&&(E-=.5*o),E=An(E/24),v>50&&!s.exact||u>0||t){if(j=0,r)switch(g){case"two-sided":j=.5*Ln(T-w);break;case"less":j=-.5;break;default:j=.5}p=(T-w-j)/E,l="two-sided"===g?2*(1-vt(ln(p))):"greater"===g?1-vt(p):vt(p)}else p=T,l="two-sided"===g?p>v*(v+1)/4?2*(1-In(p-1,v)):2*In(p,v):"greater"===g?1-In(p-1,v):In(p,v);return br(y={},"rejected",l<=c),br(y,"alpha",c),br(y,"pValue",l),br(y,"statistic",T),br(y,"nullValue",b),br(y,"alternative",g),br(y,"method",(_?"Paired":"One-Sample")+" Wilcoxon signed rank test"),br(y,"print",ht),y}}));
//# sourceMappingURL=index.js.map

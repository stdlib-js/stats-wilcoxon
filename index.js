// Copyright (c) 2023 The Stdlib Authors. License is Apache-2.0: http://www.apache.org/licenses/LICENSE-2.0
!function(r,n){"object"==typeof exports&&"undefined"!=typeof module?module.exports=n():"function"==typeof define&&define.amd?define(n):(r="undefined"!=typeof globalThis?globalThis:r||self).wilcoxon=n()}(this,(function(){"use strict";var r="function"==typeof Object.defineProperty?Object.defineProperty:null;var n,t=Object.defineProperty,e=Object.prototype,i=e.toString,o=e.__defineGetter__,u=e.__defineSetter__,a=e.__lookupGetter__,f=e.__lookupSetter__;n=function(){try{return r({},"x",{}),!0}catch(r){return!1}}()?t:function(r,n,t){var c,l,s,v;if("object"!=typeof r||null===r||"[object Array]"===i.call(r))throw new TypeError("invalid argument. First argument must be an object. Value: `"+r+"`.");if("object"!=typeof t||null===t||"[object Array]"===i.call(t))throw new TypeError("invalid argument. Property descriptor must be an object. Value: `"+t+"`.");if((l="value"in t)&&(a.call(r,n)||f.call(r,n)?(c=r.__proto__,r.__proto__=e,delete r[n],r[n]=t.value,r.__proto__=c):r[n]=t.value),s="get"in t,v="set"in t,l&&(s||v))throw new Error("invalid argument. Cannot specify one or more accessors and a value or writable attribute in the property descriptor.");return s&&o&&o.call(r,n,t.get),v&&u&&u.call(r,n,t.set),r};var c=n;function l(r,n,t){c(r,n,{configurable:!1,enumerable:!1,writable:!1,value:t})}var s=Math.floor;function v(r){return s(r)===r}function p(r){if("function"!=typeof r)throw new TypeError("invalid argument. Must provide a function. Value: `"+r+"`.");return function(n){var t,e;if(!function(r){return null!=r&&"function"!=typeof r&&"number"==typeof r.length&&v(r.length)&&r.length>=0&&r.length<=4294967295}(n))return!1;if(0===(t=n.length))return!1;for(e=0;e<t;e++)if(!1===r(n[e]))return!1;return!0}}function y(r){return"number"==typeof r}var h="function"==typeof Symbol&&"symbol"==typeof Symbol("foo");function g(){return h&&"symbol"==typeof Symbol.toStringTag}var d=Object.prototype.toString;var m=Object.prototype.hasOwnProperty;function b(r,n){return null!=r&&m.call(r,n)}var w="function"==typeof Symbol?Symbol.toStringTag:"";var j=g()?function(r){var n,t,e;if(null==r)return d.call(r);t=r[w],n=b(r,w);try{r[w]=void 0}catch(n){return d.call(r)}return e=d.call(r),n?r[w]=t:delete r[w],e}:function(r){return d.call(r)},E=Number,A=E.prototype.toString;var O=g();function T(r){return"object"==typeof r&&(r instanceof E||(O?function(r){try{return A.call(r),!0}catch(r){return!1}}(r):"[object Number]"===j(r)))}function N(r){return y(r)||T(r)}l(N,"isPrimitive",y),l(N,"isObject",T);var _=p(N.isPrimitive),x=p(N.isObject),U=p(N);l(U,"primitives",_),l(U,"objects",x);var V=Number.POSITIVE_INFINITY,P=E.NEGATIVE_INFINITY;function S(r){return r<V&&r>P&&v(r)}function M(r){return y(r)&&S(r)}function F(r){return T(r)&&S(r.valueOf())}function I(r){return M(r)||F(r)}function k(r){return M(r)&&r>=0}function z(r){return F(r)&&r.valueOf()>=0}function H(r){return k(r)||z(r)}l(I,"isPrimitive",M),l(I,"isObject",F),l(H,"isPrimitive",k),l(H,"isObject",z);var L=9007199254740991;function B(r){return null!==r&&"object"==typeof r&&k(r.length)&&r.length<=L&&"number"==typeof r.BYTES_PER_ELEMENT&&"number"==typeof r.byteOffset&&"number"==typeof r.byteLength}function G(r,n,t){c(r,n,{configurable:!1,enumerable:!0,writable:!1,value:t})}var W=Array.isArray?Array.isArray:function(r){return"[object Array]"===j(r)};function R(r){return"object"==typeof r&&null!==r&&!W(r)}var C=/./;function Y(r){return"boolean"==typeof r}var q=Boolean.prototype.toString;var X=g();function D(r){return"object"==typeof r&&(r instanceof Boolean||(X?function(r){try{return q.call(r),!0}catch(r){return!1}}(r):"[object Boolean]"===j(r)))}function Q(r){return Y(r)||D(r)}function J(){return new Function("return this;")()}l(Q,"isPrimitive",Y),l(Q,"isObject",D);var K="object"==typeof self?self:null,Z="object"==typeof window?window:null,$="object"==typeof global?global:null;var rr=function(r){if(arguments.length){if(!Y(r))throw new TypeError("invalid argument. Must provide a boolean primitive. Value: `"+r+"`.");if(r)return J()}if(K)return K;if(Z)return Z;if($)return $;throw new Error("unexpected error. Unable to resolve global object.")}(),nr=rr.document&&rr.document.childNodes,tr=Int8Array;function er(){return/^\s*function\s*([^(]*)/i}var ir=/^\s*function\s*([^(]*)/i;function or(r){return null!==r&&"object"==typeof r}function ur(r){var n,t,e,i;if(("Object"===(t=j(r).slice(8,-1))||"Error"===t)&&r.constructor){if("string"==typeof(e=r.constructor).name)return e.name;if(n=ir.exec(e.toString()))return n[1]}return or(i=r)&&(i._isBuffer||i.constructor&&"function"==typeof i.constructor.isBuffer&&i.constructor.isBuffer(i))?"Buffer":t}l(er,"REGEXP",ir),l(or,"isObjectLikeArray",function(r){if("function"!=typeof r)throw new TypeError("invalid argument. Must provide a function. Value: `"+r+"`.");return function(n){var t,e;if(!W(n))return!1;if(0===(t=n.length))return!1;for(e=0;e<t;e++)if(!1===r(n[e]))return!1;return!0}}(or));var ar="function"==typeof C||"object"==typeof tr||"function"==typeof nr?function(r){return ur(r).toLowerCase()}:function(r){var n;return null===r?"null":"object"===(n=typeof r)?ur(r).toLowerCase():n};function fr(r){return"function"===ar(r)}var cr,lr=Object.getPrototypeOf;cr=fr(Object.getPrototypeOf)?lr:function(r){var n=function(r){return r.__proto__}(r);return n||null===n?n:"[object Function]"===j(r.constructor)?r.constructor.prototype:r instanceof Object?Object.prototype:null};var sr=cr;var vr=Object.prototype;function pr(r){var n;return!!R(r)&&(n=function(r){return null==r?null:(r=Object(r),sr(r))}(r),!n||!b(r,"constructor")&&b(n,"constructor")&&fr(n.constructor)&&"[object Function]"===j(n.constructor)&&b(n,"isPrototypeOf")&&fr(n.isPrototypeOf)&&(n===vr||function(r){var n;for(n in r)if(!b(r,n))return!1;return!0}(r)))}function yr(r){return"object"==typeof r&&null!==r&&"number"==typeof r.length&&v(r.length)&&r.length>=0&&r.length<=L}function hr(r){return"string"==typeof r}var gr=String.prototype.valueOf;var dr=g();function mr(r){return"object"==typeof r&&(r instanceof String||(dr?function(r){try{return gr.call(r),!0}catch(r){return!1}}(r):"[object String]"===j(r)))}function br(r){return hr(r)||mr(r)}function wr(r){return r!=r}function jr(r){return y(r)&&wr(r)}function Er(r){return T(r)&&wr(r.valueOf())}function Ar(r){return jr(r)||Er(r)}function Or(r,n,t){var e,i,o;if(!yr(r)&&!hr(r))throw new TypeError("invalid argument. First argument must be array-like. Value: `"+r+"`.");if(arguments.length<2)throw new Error("insufficient input arguments. Must provide a search value.");if(arguments.length>2){if(!M(t))throw new TypeError("invalid argument. Third argument must be an integer. Value: `"+t+"`.");(i=t)<0&&(i=0)}else i=0;if(hr(r)){if(!hr(n))throw new TypeError("invalid argument. Second argument must be a string primitive. Value: `"+n+"`.");return-1!==r.indexOf(n,i)}if(e=r.length,jr(n)){for(o=i;o<e;o++)if(jr(r[o]))return!0;return!1}for(o=i;o<e;o++)if(r[o]===n)return!0;return!1}function Tr(r){var n,t,e;for(n=r.length,t=0,e=0;e<n;e++)t+=r[e];return t}function Nr(r){var n,t;for(n=new Array(r.length),t=0;t<r.length;t++)n[t]=t;return n.sort((function(n,t){return function(r,n){return r<n?-1:r>n?1:0}(r[n],r[t])}))}function _r(r,n){var t,e,i;for(t=r.length,e=new Array(t),i=0;i<t;i++)e[i]=Or(n,r[i]);return e}l(br,"isPrimitive",hr),l(br,"isObject",mr),l(Ar,"isPrimitive",jr),l(Ar,"isObject",Er);var xr=["min","max","average","dense","ordinal"],Ur=["last","first","remove"];function Vr(r,n){return R(n)?b(n,"encoding")&&(r.encoding=n.encoding,!W(r.encoding))?new TypeError("invalid option. `encoding` option must be an array. Option: `"+r.encoding+"`."):!b(n,"method")||(r.method=n.method,hr(r.method)&&Or(xr,r.method))?!b(n,"missing")||(r.missing=n.missing,hr(r.missing)&&Or(Ur,r.missing))?null:new TypeError("invalid option. `missing` must be one of the following values: `last`, `first`, or `remove`. Option: `"+r.missing+"`."):new TypeError("invalid option. `method` must be one of the following values: `average`, `min`, `max`, `dense`, or `ordinal`. Option: `"+r.method+"`."):new TypeError("invalid argument. Options argument must be an object. Value: `"+n+"`.")}function Pr(r,n){var t,e,i,o,u,a,f,c,l,s,v,p,y,h,g,d,m,b;if(!yr(r))throw new TypeError("invalid argument. First argument `x` must be an array-like object. Value: `"+r+"`.");if(y={},arguments.length>1&&(g=Vr(y,n)))throw g;for(v=y.method||"average",a=y.encoding||[null,NaN],l=y.missing||"last",d=r.length,h=[],m=0;m<d;m++)Or(a,r[m])||h.push(r[m]);if(t=_r(r,a),d=h.length,o=0,p=new Array(d),c=Nr(h),"ordinal"===v)for(m=0;m<d;m++)p[c[m]]=m+1;else for(e=0,m=0;m<d;m++)if(f=m+1,m===d-1||h[c[m]]!==h[c[f]]){switch(v){case"average":default:s=f-.5*e;break;case"min":s=f-e;break;case"max":s=f;break;case"dense":s=f-e-o,o+=e}for(b=m-e;b<f;b++)p[c[b]]=s;e=0}else e+=1;if("first"===l){for(i=Tr(t),b=1,u=new Array(t.length),m=0;m<t.length;m++)t[m]?(u[m]=b,b+=1):u[m]=p.shift()+i;return u}if("last"===l){for(u=new Array(t.length),m=0;m<t.length;m++)t[m]?u[m]=m+p.length+1:u[m]=p.shift();return u}return p}var Sr=Math.ceil;function Mr(r){return r<0?Sr(r):s(r)}var Fr=1023;function Ir(r){return r===V||r===P}var kr=2147483647,zr="function"==typeof Uint32Array;var Hr="function"==typeof Uint32Array?Uint32Array:null;var Lr,Br="function"==typeof Uint32Array?Uint32Array:void 0;Lr=function(){var r,n,t;if("function"!=typeof Hr)return!1;try{n=new Hr(n=[1,3.14,-3.14,4294967296,4294967297]),t=n,r=(zr&&t instanceof Uint32Array||"[object Uint32Array]"===j(t))&&1===n[0]&&3===n[1]&&4294967293===n[2]&&0===n[3]&&1===n[4]}catch(n){r=!1}return r}()?Br:function(){throw new Error("not implemented")};var Gr=Lr,Wr="function"==typeof Float64Array;var Rr="function"==typeof Float64Array?Float64Array:null;var Cr,Yr="function"==typeof Float64Array?Float64Array:void 0;Cr=function(){var r,n,t;if("function"!=typeof Rr)return!1;try{n=new Rr([1,3.14,-3.14,NaN]),t=n,r=(Wr&&t instanceof Float64Array||"[object Float64Array]"===j(t))&&1===n[0]&&3.14===n[1]&&-3.14===n[2]&&n[3]!=n[3]}catch(n){r=!1}return r}()?Yr:function(){throw new Error("not implemented")};var qr=Cr,Xr="function"==typeof Uint8Array;var Dr="function"==typeof Uint8Array?Uint8Array:null;var Qr,Jr="function"==typeof Uint8Array?Uint8Array:void 0;Qr=function(){var r,n,t;if("function"!=typeof Dr)return!1;try{n=new Dr(n=[1,3.14,-3.14,256,257]),t=n,r=(Xr&&t instanceof Uint8Array||"[object Uint8Array]"===j(t))&&1===n[0]&&3===n[1]&&253===n[2]&&0===n[3]&&1===n[4]}catch(n){r=!1}return r}()?Jr:function(){throw new Error("not implemented")};var Kr=Qr,Zr="function"==typeof Uint16Array;var $r="function"==typeof Uint16Array?Uint16Array:null;var rn,nn="function"==typeof Uint16Array?Uint16Array:void 0;rn=function(){var r,n,t;if("function"!=typeof $r)return!1;try{n=new $r(n=[1,3.14,-3.14,65536,65537]),t=n,r=(Zr&&t instanceof Uint16Array||"[object Uint16Array]"===j(t))&&1===n[0]&&3===n[1]&&65533===n[2]&&0===n[3]&&1===n[4]}catch(n){r=!1}return r}()?nn:function(){throw new Error("not implemented")};var tn,en={uint16:rn,uint8:Kr};(tn=new en.uint16(1))[0]=4660;var on,un,an=52===new en.uint8(tn.buffer)[0];!0===an?(on=1,un=0):(on=0,un=1);var fn={HIGH:on,LOW:un},cn=new qr(1),ln=new Gr(cn.buffer),sn=fn.HIGH,vn=fn.LOW;function pn(r,n,t,e){return cn[0]=r,n[e]=ln[sn],n[e+t]=ln[vn],n}function yn(r){return pn(r,[0,0],1,0)}l(yn,"assign",pn);var hn,gn,dn=!0===an?1:0,mn=new qr(1),bn=new Gr(mn.buffer);function wn(r){return mn[0]=r,bn[dn]}!0===an?(hn=1,gn=0):(hn=0,gn=1);var jn={HIGH:hn,LOW:gn},En=new qr(1),An=new Gr(En.buffer),On=jn.HIGH,Tn=jn.LOW;function Nn(r,n){return An[On]=r,An[Tn]=n,En[0]}var _n=[0,0];function xn(r,n){var t,e;return yn.assign(r,_n,1,0),t=_n[0],t&=kr,e=wn(n),Nn(t|=e&=2147483648,_n[1])}function Un(r){return Math.abs(r)}function Vn(r,n,t,e){return wr(r)||Ir(r)?(n[e]=r,n[e+t]=0,n):0!==r&&Un(r)<22250738585072014e-324?(n[e]=4503599627370496*r,n[e+t]=-52,n):(n[e]=r,n[e+t]=0,n)}l((function(r){return Vn(r,[0,0],1,0)}),"assign",Vn);var Pn=[0,0],Sn=[0,0];function Mn(r,n){var t,e;return 0===n||0===r||wr(r)||Ir(r)?r:(Vn(r,Pn,1,0),n+=Pn[1],n+=function(r){var n=wn(r);return(n=(2146435072&n)>>>20)-Fr|0}(r=Pn[0]),n<-1074?xn(0,r):n>1023?r<0?P:V:(n<=-1023?(n+=52,e=2220446049250313e-31):e=1,yn.assign(r,Sn,1,0),t=Sn[0],t&=2148532223,e*Nn(t|=n+Fr<<20,Sn[1])))}var Fn=1.4426950408889634,In=1/(1<<28);function kn(r){var n;return wr(r)||r===V?r:r===P?0:r>709.782712893384?V:r<-745.1332191019411?0:r>-3.725290298461914e-9&&r<In?1+r:function(r,n,t){var e,i,o,u;return Mn(1-(n-(e=r-n)*(o=e-(i=e*e)*(0===(u=i)?.16666666666666602:.16666666666666602+u*(u*(6613756321437934e-20+u*(4.1381367970572385e-8*u-16533902205465252e-22))-.0027777777777015593)))/(2-o)-r),t)}(r-.6931471803691238*(n=Mr(r<0?Fn*r-.5:Fn*r+.5)),1.9082149292705877e-10*n,n)}var zn=!0===an?0:1,Hn=new qr(1),Ln=new Gr(Hn.buffer);function Bn(r,n){return Hn[0]=r,Ln[zn]=n>>>0,Hn[0]}var Gn=.8450629115104675;function Wn(r){var n,t,e,i,o,u,a,f;if(wr(r))return NaN;if(r===V)return 0;if(r===P)return 2;if(0===r)return 1;if(r<0?(n=!0,t=-r):(n=!1,t=r),t<.84375)return t<13877787807814457e-33?1-r:(i=.12837916709551256+(e=r*r)*function(r){return 0===r?-.3250421072470015:r*(r*(-23763016656650163e-21*r-.005770270296489442)-.02848174957559851)-.3250421072470015}(e),o=1+e*function(r){return 0===r?.39791722395915535:.39791722395915535+r*(.0650222499887673+r*(.005081306281875766+r*(.00013249473800432164+-3960228278775368e-21*r)))}(e),u=i/o,r<.25?1-(r+r*u):(i=r*u,.5-(i+=r-.5)));if(t<1.25)return a=(o=t-1)*function(r){return 0===r?.41485611868374833:.41485611868374833+r*(r*(.31834661990116175+r*(r*(.035478304325618236+-.002166375594868791*r)-.11089469428239668))-.3722078760357013)}(o)-.0023621185607526594,f=1+o*function(r){return 0===r?.10642088040084423:.10642088040084423+r*(.540397917702171+r*(.07182865441419627+r*(.12617121980876164+r*(.01363708391202905+.011984499846799107*r))))}(o),n?1+Gn+a/f:1-Gn-a/f;if(t<28){if(o=1/(t*t),t<2.857142857142857)i=o*function(r){return 0===r?-.6938585727071818:r*(r*(r*(r*(r*(-9.814329344169145*r-81.2874355063066)-184.60509290671104)-162.39666946257347)-62.375332450326006)-10.558626225323291)-.6938585727071818}(o)-.009864944034847148,o=1+o*function(r){return 0===r?19.651271667439257:19.651271667439257+r*(137.65775414351904+r*(434.56587747522923+r*(645.3872717332679+r*(429.00814002756783+r*(108.63500554177944+r*(6.570249770319282+-.0604244152148581*r))))))}(o);else{if(r<-6)return 2;i=o*function(r){return 0===r?-.799283237680523:r*(r*(r*(r*(-483.5191916086514*r-1025.0951316110772)-637.5664433683896)-160.63638485582192)-17.757954917754752)-.799283237680523}(o)-.0098649429247001,o=1+o*function(r){return 0===r?30.33806074348246:30.33806074348246+r*(325.7925129965739+r*(1536.729586084437+r*(3199.8582195085955+r*(2553.0504064331644+r*(474.52854120695537+-22.44095244658582*r)))))}(o)}return i=kn(-(e=Bn(t,0))*e-.5625)*kn((e-t)*(e+t)+i/o),n?2-i/t:i/t}return n?2:0}var Rn=Math.sqrt;function Cn(r){return function(){return r}}function Yn(r){return wr(r)?Cn(NaN):function(n){if(wr(n))return NaN;return n<r?0:1}}function qn(r,n){var t;return wr(r)||wr(n)||n<0?Cn(NaN):0===n?Yn(r):(t=n*Rn(2),function(n){if(wr(n))return NaN;return.5*Wn(-(n-r)/t)})}function Xn(r){return s(r)===r&&r>0}function Dn(r){return r==r&&r>P&&r<V}l((function(r,n){return wr(r)||wr(n)?NaN:r<n?0:1}),"factory",Yn),l((function(r,n,t){return wr(r)||wr(n)||wr(t)||t<0?NaN:0===t?r<n?0:1:.5*Wn(-(r-n)/(t*Rn(2)))}),"factory",qn);var Qn,Jn=Math.round,Kn=.6931471805599453;function Zn(r){return r}Qn=function(r,n){var t,e;if(!fr(r))throw new TypeError("invalid argument. First argument must be a function. Value: `"+r+"`.");if(arguments.length<2)t=Zn;else if(!fr(t=n))throw new TypeError("invalid argument. Hash function argument must be a function. Value: `"+t+"`.");return l(i,"cache",e={}),i;function i(){var n,i,o,u;for(n=new Array(arguments.length),u=0;u<arguments.length;u++)n[u]=arguments[u];return o=t(n).toString(),b(e,o)?e[o]:(i=r.apply(null,n),e[o]=i,i)}}((function(r,n){var t;return 0===n?0===r?1:0:(t=n*(n+1)/2,r<0||r>t?0:(r>t/2&&(r=t-r),Qn(r-n,n-1)+Qn(r,n-1)))}));var $n=Qn;function rt(r,n){var t,e,i;if(wr(r)||!Xn(n)||!Dn(n))return NaN;if(r<0)return 0;if((r=Jn(r))>=n*(n+1)/2)return 1;for(t=kn(-n*Kn),i=0,e=0;e<=r;e++)i+=$n(e,n)*t;return i}function nt(r,n,t){var e,i;if(!yr(r)&&!hr(r))throw new TypeError("invalid argument. First argument must be an array-like object. Value: `"+r+"`.");if(0===(e=r.length))return-1;if(3===arguments.length){if(!M(t))throw new TypeError("invalid argument. `fromIndex` must be an integer. Value: `"+t+"`.");if(t>=0){if(t>=e)return-1;i=t}else(i=e+t)<0&&(i=0)}else i=0;if(Ar(n)){for(;i<e;i++)if(Ar(r[i]))return i}else for(;i<e;i++)if(r[i]===n)return i;return-1}function tt(r){var n,t,e,i,o,u,a;if(!yr(r))throw new TypeError("invalid argument. First argument must be a collection. Value: `"+r+"`.");for(n=0,t=[],i=[],e=r.length,u=0;u<e;u++)n+=1,-1===(a=nt(t,o=r[u]))?(t.push(o),i.push([o,1,0])):i[a][1]+=1;for(e=i.length,u=0;u<e;u++)i[u][2]=i[u][1]/n;return i}function et(r){return 0===r||wr(r)?r:r<0?-1:1}function it(){var r,n=arguments,t=n[0],e="https://stdlib.io/e/"+t+"?";for(r=1;r<n.length;r++)e+="&arg[]="+encodeURIComponent(n[r]);return e}l(rt,"factory",(function(r){var n,t;return Xn(r)&&Dn(r)?(t=kn(-r*Kn),n=r*(r+1)/2,function(e){var i,o;if(wr(e))return NaN;if(e<0)return 0;if((e=Jn(e))>=n)return 1;for(o=0,i=0;i<=e;i++)o+=$n(i,r)*t;return o}):Cn(NaN)}));var ot=["two-sided","less","greater"],ut=["pratt","wilcox","zsplit"];function at(r,n){if(!pr(n))return new TypeError(it("0fs2h",n));if(b(n,"alpha")){if(r.alpha=n.alpha,!y(r.alpha)||Ar(r.alpha))return new TypeError(it("0fs8h","alpha",r.alpha));if(r.alpha<0||r.alpha>1)return new RangeError(it("invalid option. `%s` option must be a number on the interval: [0, 1]. Option: `%f`.","alpha",r.alpha))}if(b(n,"alternative")){if(r.alternative=n.alternative,!hr(r.alternative))return new TypeError(it("0fs2i","alternative",r.alternative));if(!Or(ot,r.alternative))return new Error(it("0fs3t","alternative",ot.join('", "'),r.alternative))}if(b(n,"correction")&&(r.correction=n.correction,!Y(r.correction)||Ar(r.correction)))return new TypeError(it("0fs30","correction",r.alpha));if(b(n,"exact")&&(r.exact=n.exact,!Y(r.exact)||Ar(r.exact)))return new TypeError(it("0fs30","exact",r.alpha));if(b(n,"mu")&&(r.mu=n.mu,!y(r.mu)||Ar(r.mu)))return new TypeError(it("0fs8h","mu",r.mu));if(b(n,"zeroMethod")){if(r.zeroMethod=n.zeroMethod,!hr(r.zeroMethod))return new TypeError(it("0fs2i","zeroMethod",r.alternative));if(!Or(ut,r.zeroMethod))return new Error(it("0fs3t","zeroMethod",ut.join('", "'),r.zeroMethod))}return null}function ft(r,n){return r-n}function ct(r){var n,t,e,i;for((r=r.slice()).sort(ft),n=r.length,e=1,i=0;e<n;e++)t=r[e],r[i]!==t&&(r[i+=1]=t);return r.length=i+1,r}function lt(r){return M(r)&&r>0}function st(r){return F(r)&&r.valueOf()>0}function vt(r){return lt(r)||st(r)}function pt(r){return v(r/2)}function yt(r){return pt(r>0?r-1:r+1)}function ht(r){return 0|r}l(vt,"isPrimitive",lt),l(vt,"isObject",st);var gt=!0===an?1:0,dt=new qr(1),mt=new Gr(dt.buffer);function bt(r,n){return dt[0]=r,mt[gt]=n>>>0,dt[0]}var wt=1048576,jt=[1,1.5],Et=[0,.5849624872207642],At=[0,1.350039202129749e-8];var Ot=1048575;var Tt=1048576;var Nt=1083179008,_t=1e300,xt=1e-300,Ut=[0,0],Vt=[0,0];function Pt(r,n){var t,e,i,o,u,a,f,c,l,s,p,y,h,g;if(wr(r)||wr(n))return NaN;if(yn.assign(n,Ut,1,0),u=Ut[0],0===Ut[1]){if(0===n)return 1;if(1===n)return r;if(-1===n)return 1/r;if(.5===n)return Rn(r);if(-.5===n)return 1/Rn(r);if(2===n)return r*r;if(3===n)return r*r*r;if(4===n)return(r*=r)*r;if(Ir(n))return function(r,n){return-1===r?(r-r)/(r-r):1===r?1:Un(r)<1==(n===V)?0:V}(r,n)}if(yn.assign(r,Ut,1,0),o=Ut[0],0===Ut[1]){if(0===o)return function(r,n){return n===P?V:n===V?0:n>0?yt(n)?r:0:yt(n)?xn(V,r):V}(r,n);if(1===r)return 1;if(-1===r&&yt(n))return-1;if(Ir(r))return r===P?Pt(-0,-n):n<0?0:V}if(r<0&&!1===v(n))return(r-r)/(r-r);if(i=Un(r),t=o&kr|0,e=u&kr|0,f=u>>>31|0,a=(a=o>>>31|0)&&yt(n)?-1:1,e>1105199104){if(e>1139802112)return function(r,n){return(wn(r)&kr)<=1072693247?n<0?1/0:0:n>0?1/0:0}(r,n);if(t<1072693247)return 1===f?a*_t*_t:a*xt*xt;if(t>1072693248)return 0===f?a*_t*_t:a*xt*xt;p=function(r,n){var t,e,i,o,u,a;return t=(u=1.9259629911266175e-8*(i=n-1)-i*i*(0===(a=i)?.5:.5+a*(.25*a-.3333333333333333))*1.4426950408889634)-((e=Bn(e=(o=1.4426950216293335*i)+u,0))-o),r[0]=e,r[1]=t,r}(Vt,i)}else p=function(r,n,t){var e,i,o,u,a,f,c,l,s,v,p,y,h,g,d,m,b,w,j,E,A;return w=0,t<wt&&(w-=53,t=wn(n*=9007199254740992)),w+=(t>>20)-Fr|0,t=1072693248|(j=1048575&t|0),j<=235662?E=0:j<767610?E=1:(E=0,w+=1,t-=wt),u=Bn(i=(m=(n=bt(n,t))-(c=jt[E]))*(b=1/(n+c)),0),e=524288+(t>>1|536870912),f=bt(0,e+=E<<18),d=(o=i*i)*o*(0===(A=o)?.5999999999999946:.5999999999999946+A*(.4285714285785502+A*(.33333332981837743+A*(.272728123808534+A*(.23066074577556175+.20697501780033842*A))))),f=Bn(f=3+(o=u*u)+(d+=(a=b*(m-u*f-u*(n-(f-c))))*(u+i)),0),h=(p=-7.028461650952758e-9*(s=Bn(s=(m=u*f)+(b=a*f+(d-(f-3-o))*i),0))+.9617966939259756*(b-(s-m))+At[E])-((y=Bn(y=(v=.9617967009544373*s)+p+(l=Et[E])+(g=w),0))-g-l-v),r[0]=y,r[1]=h,r}(Vt,i,t);if(y=(s=(n-(c=Bn(n,0)))*p[0]+n*p[1])+(l=c*p[0]),yn.assign(y,Ut,1,0),h=ht(Ut[0]),g=ht(Ut[1]),h>=Nt){if(0!=(h-Nt|g))return a*_t*_t;if(s+8008566259537294e-32>y-l)return a*_t*_t}else if((h&kr)>=1083231232){if(0!=(h-3230714880|g))return a*xt*xt;if(s<=y-l)return a*xt*xt}return y=function(r,n,t){var e,i,o,u,a,f,c,l,s,v;return s=((l=r&kr|0)>>20)-Fr|0,c=0,l>1071644672&&(i=bt(0,((c=r+(Tt>>s+1)>>>0)&~(Ot>>(s=((c&kr)>>20)-Fr|0)))>>>0),c=(c&Ot|Tt)>>20-s>>>0,r<0&&(c=-c),n-=i),r=ht(r=wn(f=1-((f=(o=.6931471824645996*(i=Bn(i=t+n,0)))+(u=(t-(i-n))*Kn+-1.904654299957768e-9*i))*(e=f-(i=f*f)*(0===(v=i)?.16666666666666602:.16666666666666602+v*(v*(6613756321437934e-20+v*(4.1381367970572385e-8*v-16533902205465252e-22))-.0027777777777015593)))/(e-2)-((a=u-(f-o))+f*a)-f))),(r+=c<<20>>>0)>>20<=0?Mn(f,c):bt(f,r)}(h,l,s),a*y}var St=1e308;function Mt(r,n){var t,e;return wr(r)||wr(n)||Ir(n)?NaN:Ir(r)||0===r||n<-324||Un(r)>9007199254740992&&n<=0?r:n>308?0*r:n<-308?(t=Pt(10,-(n+308)),Ir(e=r*St*t)?r:Jn(e)/St/t):Ir(e=r*(t=Pt(10,-n)))?r:Jn(e)/t}function Ft(r){var n,t,e;if(t=4,n=!0,arguments.length>0){if(!pr(r))throw new TypeError(it("0fs3X",r));if(b(r,"digits")){if(!vt(r.digits))throw new TypeError(it("0fs3b","digits",r.digits));t=r.digits}if(b(r,"decision")){if(!Y(r.decision))throw new TypeError(it("0fs30","decision",r.decision));n=r.decision}}switch(e="",e+=this.method,e+="\n\n",e+="Alternative hypothesis: ","Paired Wilcoxon signed rank test"===this.method?e+="Median of the difference `x - y` is ":e+="Median of `x` is ",this.alternative){case"less":e+="less than ";break;case"greater":e+="greater than ";break;default:e+="not equal to "}return e+=this.nullValue,e+="\n\n",e+="    pValue: "+Mt(this.pValue,-t)+"\n",e+="    statistic: "+Mt(this.statistic,-t)+"\n",e+="\n",n&&(e+="Test Decision: ",this.rejected?e+="Reject null in favor of alternative at "+100*this.alpha+"% significance level":e+="Fail to reject null in favor of alternative at "+100*this.alpha+"% significance level",e+="\n"),e}var It=qn(0,1);return function(){var r,n,t,e,i,o,u,a,f,c,l,s,v,p,y,h,g,d,m,b,w,j,E,A,O,T,N,x,U;if(!B(x=arguments[0])&&!_(x))throw new TypeError(it("0fs8j",x));if(h=x.length,arguments.length>1)if(pr(arguments[1]))t=arguments[1];else{if(!B(U=arguments[1])&&!_(U))throw new TypeError(it("invalid argument. `%s` argument must be a numeric array. Value: `%s`.","y",U));if(h!==U.length)throw new Error(it("0fs1H"));arguments.length>2&&(t=arguments[2])}if(s={},t&&(y=at(s,t)))throw y;if(b=s.mu||0,r=void 0===s.correction||s.correction,c=void 0===s.alpha?.05:s.alpha,h<2)throw new Error(it("0fsAn",x));if(p=s.alternative||"two-sided","wilcox"===(n=s.zeroMethod||"wilcox")){if(E=[],U)for(A=0;A<h;A++)0!==(N=x[A]-U[A]-b)&&E.push(N);else for(A=0;A<h;A++)0!==x[A]&&E.push(x[A]-b);a=x.length-E.length}else if(E=new qr(h),a=0,U)for(A=0;A<h;A++)E[A]=x[A]-U[A]-b,0===E[A]&&(a+=1);else for(A=0;A<h;A++)E[A]=x[A]-b,0===E[A]&&(a+=1);if(a===h)throw new Error(it("0fs1Q"));for(h=E.length,m=new qr(h),A=0;A<h;A++)m[A]=Un(E[A]);for(O=Pr(m),u=0,f=0,A=0;A<h;A++)E[A]>0?u+=O[A]:0===E[A]&&(f+=O[A]);if(e=ct(O).length!==O.length,"zsplit"===n&&(u+=f/2),T=u,w=h*(h+1)*.25,j=h*(h+1)*(2*h+1),"pratt"===n){for(g=[],A=0;A<h;A++)0!==E[A]&&g.push(O[A]);O=g,w-=a*(a+1)*.25,j-=a*(a+1)*(2*a+1)}for(i=tt(O),o=0,A=0;A<i.length;A++)i[A][1]>1&&(o+=(N=i[A][1])*(N*N-1));if(o>0&&(j-=.5*o),j=Rn(j/24),h>50&&!s.exact||a>0||e){if(E=0,r)switch(p){case"two-sided":E=.5*et(T-w);break;case"less":E=-.5;break;default:E=.5}v=(T-w-E)/j,l="two-sided"===p?2*(1-It(Un(v))):"greater"===p?1-It(v):It(v)}else v=T,l="two-sided"===p?v>h*(h+1)/4?2*(1-rt(v-1,h)):2*rt(v,h):"greater"===p?1-rt(v-1,h):rt(v,h);return G(d={},"rejected",l<=c),G(d,"alpha",c),G(d,"pValue",l),G(d,"statistic",T),G(d,"nullValue",b),G(d,"alternative",p),G(d,"method",(U?"Paired":"One-Sample")+" Wilcoxon signed rank test"),G(d,"print",Ft),d}}));
//# sourceMappingURL=index.js.map

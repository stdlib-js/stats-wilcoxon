// Copyright (c) 2022 The Stdlib Authors. License is Apache-2.0: http://www.apache.org/licenses/LICENSE-2.0
!function(r,n){"object"==typeof exports&&"undefined"!=typeof module?module.exports=n():"function"==typeof define&&define.amd?define(n):(r="undefined"!=typeof globalThis?globalThis:r||self).wilcoxon=n()}(this,(function(){"use strict";var r="function"==typeof Object.defineProperty?Object.defineProperty:null;var n,t=Object.defineProperty,e=Object.prototype,i=e.toString,o=e.__defineGetter__,u=e.__defineSetter__,f=e.__lookupGetter__,a=e.__lookupSetter__;n=function(){try{return r({},"x",{}),!0}catch(r){return!1}}()?t:function(r,n,t){var c,l,s,p;if("object"!=typeof r||null===r||"[object Array]"===i.call(r))throw new TypeError("invalid argument. First argument must be an object. Value: `"+r+"`.");if("object"!=typeof t||null===t||"[object Array]"===i.call(t))throw new TypeError("invalid argument. Property descriptor must be an object. Value: `"+t+"`.");if((l="value"in t)&&(f.call(r,n)||a.call(r,n)?(c=r.__proto__,r.__proto__=e,delete r[n],r[n]=t.value,r.__proto__=c):r[n]=t.value),s="get"in t,p="set"in t,l&&(s||p))throw new Error("invalid argument. Cannot specify one or more accessors and a value or writable attribute in the property descriptor.");return s&&o&&o.call(r,n,t.get),p&&u&&u.call(r,n,t.set),r};var c=n;function l(r,n,t){c(r,n,{configurable:!1,enumerable:!1,writable:!1,value:t})}var s=Math.floor;function p(r){return s(r)===r}function v(r){if("function"!=typeof r)throw new TypeError("invalid argument. Must provide a function. Value: `"+r+"`.");return function(n){var t,e;if(!function(r){return null!=r&&"function"!=typeof r&&"number"==typeof r.length&&p(r.length)&&r.length>=0&&r.length<=4294967295}(n))return!1;if(0===(t=n.length))return!1;for(e=0;e<t;e++)if(!1===r(n[e]))return!1;return!0}}function y(r){return"number"==typeof r}var h="function"==typeof Symbol&&"symbol"==typeof Symbol("foo");function g(){return h&&"symbol"==typeof Symbol.toStringTag}var d=Object.prototype.toString;var m=Object.prototype.hasOwnProperty;function w(r,n){return null!=r&&m.call(r,n)}var b="function"==typeof Symbol?Symbol.toStringTag:"";var j=g()?function(r){var n,t,e;if(null==r)return d.call(r);t=r[b],n=w(r,b);try{r[b]=void 0}catch(n){return d.call(r)}return e=d.call(r),n?r[b]=t:delete r[b],e}:function(r){return d.call(r)},E=Number,A=E.prototype.toString;var O=g();function T(r){return"object"==typeof r&&(r instanceof E||(O?function(r){try{return A.call(r),!0}catch(r){return!1}}(r):"[object Number]"===j(r)))}function N(r){return y(r)||T(r)}l(N,"isPrimitive",y),l(N,"isObject",T);var _=v(N.isPrimitive),x=v(N.isObject),U=v(N);l(U,"primitives",_),l(U,"objects",x);var V=Number.POSITIVE_INFINITY,P=E.NEGATIVE_INFINITY;function S(r){return r<V&&r>P&&p(r)}function M(r){return y(r)&&S(r)}function F(r){return T(r)&&S(r.valueOf())}function I(r){return M(r)||F(r)}function k(r){return M(r)&&r>=0}function z(r){return F(r)&&r.valueOf()>=0}function H(r){return k(r)||z(r)}l(I,"isPrimitive",M),l(I,"isObject",F),l(H,"isPrimitive",k),l(H,"isObject",z);var L=9007199254740991;function B(r){return null!==r&&"object"==typeof r&&k(r.length)&&r.length<=L&&"number"==typeof r.BYTES_PER_ELEMENT&&"number"==typeof r.byteOffset&&"number"==typeof r.byteLength}function G(r,n,t){c(r,n,{configurable:!1,enumerable:!0,writable:!1,value:t})}var W=Array.isArray?Array.isArray:function(r){return"[object Array]"===j(r)};function R(r){return"object"==typeof r&&null!==r&&!W(r)}var C=/./;function Y(r){return"boolean"==typeof r}var q=Boolean.prototype.toString;var X=g();function D(r){return"object"==typeof r&&(r instanceof Boolean||(X?function(r){try{return q.call(r),!0}catch(r){return!1}}(r):"[object Boolean]"===j(r)))}function Q(r){return Y(r)||D(r)}function J(){return new Function("return this;")()}l(Q,"isPrimitive",Y),l(Q,"isObject",D);var K="object"==typeof self?self:null,Z="object"==typeof window?window:null,$="undefined"!=typeof global?global:"undefined"!=typeof self?self:"undefined"!=typeof window?window:{},rr="object"==typeof $?$:null;var nr=function(r){if(arguments.length){if(!Y(r))throw new TypeError("invalid argument. Must provide a boolean primitive. Value: `"+r+"`.");if(r)return J()}if(K)return K;if(Z)return Z;if(rr)return rr;throw new Error("unexpected error. Unable to resolve global object.")}(),tr=nr.document&&nr.document.childNodes,er=Int8Array;function ir(){return/^\s*function\s*([^(]*)/i}var or=/^\s*function\s*([^(]*)/i;function ur(r){return null!==r&&"object"==typeof r}function fr(r){var n,t,e,i;if(("Object"===(t=j(r).slice(8,-1))||"Error"===t)&&r.constructor){if("string"==typeof(e=r.constructor).name)return e.name;if(n=or.exec(e.toString()))return n[1]}return ur(i=r)&&(i._isBuffer||i.constructor&&"function"==typeof i.constructor.isBuffer&&i.constructor.isBuffer(i))?"Buffer":t}l(ir,"REGEXP",or),l(ur,"isObjectLikeArray",function(r){if("function"!=typeof r)throw new TypeError("invalid argument. Must provide a function. Value: `"+r+"`.");return function(n){var t,e;if(!W(n))return!1;if(0===(t=n.length))return!1;for(e=0;e<t;e++)if(!1===r(n[e]))return!1;return!0}}(ur));var ar="function"==typeof C||"object"==typeof er||"function"==typeof tr?function(r){return fr(r).toLowerCase()}:function(r){var n;return null===r?"null":"object"===(n=typeof r)?fr(r).toLowerCase():n};function cr(r){return"function"===ar(r)}var lr,sr=Object.getPrototypeOf;lr=cr(Object.getPrototypeOf)?sr:function(r){var n=function(r){return r.__proto__}(r);return n||null===n?n:"[object Function]"===j(r.constructor)?r.constructor.prototype:r instanceof Object?Object.prototype:null};var pr=lr;var vr=Object.prototype;function yr(r){var n;return!!R(r)&&(n=function(r){return null==r?null:(r=Object(r),pr(r))}(r),!n||!w(r,"constructor")&&w(n,"constructor")&&cr(n.constructor)&&"[object Function]"===j(n.constructor)&&w(n,"isPrototypeOf")&&cr(n.isPrototypeOf)&&(n===vr||function(r){var n;for(n in r)if(!w(r,n))return!1;return!0}(r)))}function hr(r){return"object"==typeof r&&null!==r&&"number"==typeof r.length&&p(r.length)&&r.length>=0&&r.length<=L}function gr(r){return"string"==typeof r}var dr=String.prototype.valueOf;var mr=g();function wr(r){return"object"==typeof r&&(r instanceof String||(mr?function(r){try{return dr.call(r),!0}catch(r){return!1}}(r):"[object String]"===j(r)))}function br(r){return gr(r)||wr(r)}function jr(r){return r!=r}function Er(r){return y(r)&&jr(r)}function Ar(r){return T(r)&&jr(r.valueOf())}function Or(r){return Er(r)||Ar(r)}function Tr(r,n,t){var e,i,o;if(!hr(r)&&!gr(r))throw new TypeError("invalid argument. First argument must be array-like. Value: `"+r+"`.");if(arguments.length<2)throw new Error("insufficient input arguments. Must provide a search value.");if(arguments.length>2){if(!M(t))throw new TypeError("invalid argument. Third argument must be an integer. Value: `"+t+"`.");(i=t)<0&&(i=0)}else i=0;if(gr(r)){if(!gr(n))throw new TypeError("invalid argument. Second argument must be a string primitive. Value: `"+n+"`.");return-1!==r.indexOf(n,i)}if(e=r.length,Er(n)){for(o=i;o<e;o++)if(Er(r[o]))return!0;return!1}for(o=i;o<e;o++)if(r[o]===n)return!0;return!1}function Nr(r){var n,t,e;for(n=r.length,t=0,e=0;e<n;e++)t+=r[e];return t}function _r(r){var n,t;for(n=new Array(r.length),t=0;t<r.length;t++)n[t]=t;return n.sort((function(n,t){return function(r,n){return r<n?-1:r>n?1:0}(r[n],r[t])}))}function xr(r,n){var t,e,i;for(t=r.length,e=new Array(t),i=0;i<t;i++)e[i]=Tr(n,r[i]);return e}l(br,"isPrimitive",gr),l(br,"isObject",wr),l(Or,"isPrimitive",Er),l(Or,"isObject",Ar);var Ur=["min","max","average","dense","ordinal"],Vr=["last","first","remove"];function Pr(r,n){return R(n)?w(n,"encoding")&&(r.encoding=n.encoding,!W(r.encoding))?new TypeError("invalid option. `encoding` option must be an array. Option: `"+r.encoding+"`."):!w(n,"method")||(r.method=n.method,gr(r.method)&&Tr(Ur,r.method))?!w(n,"missing")||(r.missing=n.missing,gr(r.missing)&&Tr(Vr,r.missing))?null:new TypeError("invalid option. `missing` must be one of the following values: `last`, `first`, or `remove`. Option: `"+r.missing+"`."):new TypeError("invalid option. `method` must be one of the following values: `average`, `min`, `max`, `dense`, or `ordinal`. Option: `"+r.method+"`."):new TypeError("invalid argument. Options argument must be an object. Value: `"+n+"`.")}function Sr(r,n){var t,e,i,o,u,f,a,c,l,s,p,v,y,h,g,d,m,w;if(!hr(r))throw new TypeError("invalid argument. First argument `x` must be an array-like object. Value: `"+r+"`.");if(y={},arguments.length>1&&(g=Pr(y,n)))throw g;for(p=y.method||"average",f=y.encoding||[null,NaN],l=y.missing||"last",d=r.length,h=[],m=0;m<d;m++)Tr(f,r[m])||h.push(r[m]);if(t=xr(r,f),d=h.length,o=0,v=new Array(d),c=_r(h),"ordinal"===p)for(m=0;m<d;m++)v[c[m]]=m+1;else for(e=0,m=0;m<d;m++)if(a=m+1,m===d-1||h[c[m]]!==h[c[a]]){switch(p){case"average":default:s=a-.5*e;break;case"min":s=a-e;break;case"max":s=a;break;case"dense":s=a-e-o,o+=e}for(w=m-e;w<a;w++)v[c[w]]=s;e=0}else e+=1;if("first"===l){for(i=Nr(t),w=1,u=new Array(t.length),m=0;m<t.length;m++)t[m]?(u[m]=w,w+=1):u[m]=v.shift()+i;return u}if("last"===l){for(u=new Array(t.length),m=0;m<t.length;m++)t[m]?u[m]=m+v.length+1:u[m]=v.shift();return u}return v}var Mr=Math.ceil;function Fr(r){return r<0?Mr(r):s(r)}var Ir=1023;function kr(r){return r===V||r===P}var zr="function"==typeof Uint32Array;var Hr="function"==typeof Uint32Array?Uint32Array:null;var Lr,Br="function"==typeof Uint32Array?Uint32Array:void 0;Lr=function(){var r,n,t;if("function"!=typeof Hr)return!1;try{n=new Hr(n=[1,3.14,-3.14,4294967296,4294967297]),t=n,r=(zr&&t instanceof Uint32Array||"[object Uint32Array]"===j(t))&&1===n[0]&&3===n[1]&&4294967293===n[2]&&0===n[3]&&1===n[4]}catch(n){r=!1}return r}()?Br:function(){throw new Error("not implemented")};var Gr=Lr,Wr="function"==typeof Float64Array;var Rr="function"==typeof Float64Array?Float64Array:null;var Cr,Yr="function"==typeof Float64Array?Float64Array:void 0;Cr=function(){var r,n,t;if("function"!=typeof Rr)return!1;try{n=new Rr([1,3.14,-3.14,NaN]),t=n,r=(Wr&&t instanceof Float64Array||"[object Float64Array]"===j(t))&&1===n[0]&&3.14===n[1]&&-3.14===n[2]&&n[3]!=n[3]}catch(n){r=!1}return r}()?Yr:function(){throw new Error("not implemented")};var qr=Cr,Xr="function"==typeof Uint8Array;var Dr="function"==typeof Uint8Array?Uint8Array:null;var Qr,Jr="function"==typeof Uint8Array?Uint8Array:void 0;Qr=function(){var r,n,t;if("function"!=typeof Dr)return!1;try{n=new Dr(n=[1,3.14,-3.14,256,257]),t=n,r=(Xr&&t instanceof Uint8Array||"[object Uint8Array]"===j(t))&&1===n[0]&&3===n[1]&&253===n[2]&&0===n[3]&&1===n[4]}catch(n){r=!1}return r}()?Jr:function(){throw new Error("not implemented")};var Kr=Qr,Zr="function"==typeof Uint16Array;var $r="function"==typeof Uint16Array?Uint16Array:null;var rn,nn="function"==typeof Uint16Array?Uint16Array:void 0;rn=function(){var r,n,t;if("function"!=typeof $r)return!1;try{n=new $r(n=[1,3.14,-3.14,65536,65537]),t=n,r=(Zr&&t instanceof Uint16Array||"[object Uint16Array]"===j(t))&&1===n[0]&&3===n[1]&&65533===n[2]&&0===n[3]&&1===n[4]}catch(n){r=!1}return r}()?nn:function(){throw new Error("not implemented")};var tn,en={uint16:rn,uint8:Kr};(tn=new en.uint16(1))[0]=4660;var on,un,fn=52===new en.uint8(tn.buffer)[0];!0===fn?(on=1,un=0):(on=0,un=1);var an={HIGH:on,LOW:un},cn=new qr(1),ln=new Gr(cn.buffer),sn=an.HIGH,pn=an.LOW;function vn(r,n){return cn[0]=n,r[0]=ln[sn],r[1]=ln[pn],r}function yn(r,n){return 1===arguments.length?vn([0,0],r):vn(r,n)}var hn,gn,dn=!0===fn?1:0,mn=new qr(1),wn=new Gr(mn.buffer);function bn(r){return mn[0]=r,wn[dn]}!0===fn?(hn=1,gn=0):(hn=0,gn=1);var jn={HIGH:hn,LOW:gn},En=new qr(1),An=new Gr(En.buffer),On=jn.HIGH,Tn=jn.LOW;function Nn(r,n){return An[On]=r,An[Tn]=n,En[0]}var _n=[0,0];function xn(r,n){var t,e;return yn(_n,r),t=_n[0],t&=2147483647,e=bn(n),Nn(t|=e&=2147483648,_n[1])}function Un(r){return Math.abs(r)}function Vn(r,n,t,e){return jr(r)||kr(r)?(n[e]=r,n[e+t]=0,n):0!==r&&Un(r)<22250738585072014e-324?(n[e]=4503599627370496*r,n[e+t]=-52,n):(n[e]=r,n[e+t]=0,n)}l((function(r){return Vn(r,[0,0],1,0)}),"assign",Vn);var Pn=[0,0],Sn=[0,0];function Mn(r,n){var t,e;return 0===n||0===r||jr(r)||kr(r)?r:(Vn(r,Pn,1,0),n+=Pn[1],n+=function(r){var n=bn(r);return(n=(2146435072&n)>>>20)-Ir|0}(r=Pn[0]),n<-1074?xn(0,r):n>1023?r<0?P:V:(n<=-1023?(n+=52,e=2220446049250313e-31):e=1,yn(Sn,r),t=Sn[0],t&=2148532223,e*Nn(t|=n+Ir<<20,Sn[1])))}var Fn=1.4426950408889634,In=1/(1<<28);function kn(r){var n;return jr(r)||r===V?r:r===P?0:r>709.782712893384?V:r<-745.1332191019411?0:r>-3.725290298461914e-9&&r<In?1+r:function(r,n,t){var e,i,o,u;return Mn(1-(n-(e=r-n)*(o=e-(i=e*e)*(0===(u=i)?.16666666666666602:.16666666666666602+u*(u*(6613756321437934e-20+u*(4.1381367970572385e-8*u-16533902205465252e-22))-.0027777777777015593)))/(2-o)-r),t)}(r-.6931471803691238*(n=Fr(r<0?Fn*r-.5:Fn*r+.5)),1.9082149292705877e-10*n,n)}var zn=!0===fn?0:1,Hn=new qr(1),Ln=new Gr(Hn.buffer);function Bn(r,n){return Hn[0]=r,Ln[zn]=n>>>0,Hn[0]}var Gn=.8450629115104675;function Wn(r){var n,t,e,i,o,u,f,a;if(jr(r))return NaN;if(r===V)return 0;if(r===P)return 2;if(0===r)return 1;if(r<0?(n=!0,t=-r):(n=!1,t=r),t<.84375)return t<13877787807814457e-33?1-r:(i=.12837916709551256+(e=r*r)*function(r){return 0===r?-.3250421072470015:r*(r*(-23763016656650163e-21*r-.005770270296489442)-.02848174957559851)-.3250421072470015}(e),o=1+e*function(r){return 0===r?.39791722395915535:.39791722395915535+r*(.0650222499887673+r*(.005081306281875766+r*(.00013249473800432164+-3960228278775368e-21*r)))}(e),u=i/o,r<.25?1-(r+r*u):(i=r*u,.5-(i+=r-.5)));if(t<1.25)return f=(o=t-1)*function(r){return 0===r?.41485611868374833:.41485611868374833+r*(r*(.31834661990116175+r*(r*(.035478304325618236+-.002166375594868791*r)-.11089469428239668))-.3722078760357013)}(o)-.0023621185607526594,a=1+o*function(r){return 0===r?.10642088040084423:.10642088040084423+r*(.540397917702171+r*(.07182865441419627+r*(.12617121980876164+r*(.01363708391202905+.011984499846799107*r))))}(o),n?1+Gn+f/a:1-Gn-f/a;if(t<28){if(o=1/(t*t),t<2.857142857142857)i=o*function(r){return 0===r?-.6938585727071818:r*(r*(r*(r*(r*(-9.814329344169145*r-81.2874355063066)-184.60509290671104)-162.39666946257347)-62.375332450326006)-10.558626225323291)-.6938585727071818}(o)-.009864944034847148,o=1+o*function(r){return 0===r?19.651271667439257:19.651271667439257+r*(137.65775414351904+r*(434.56587747522923+r*(645.3872717332679+r*(429.00814002756783+r*(108.63500554177944+r*(6.570249770319282+-.0604244152148581*r))))))}(o);else{if(r<-6)return 2;i=o*function(r){return 0===r?-.799283237680523:r*(r*(r*(r*(-483.5191916086514*r-1025.0951316110772)-637.5664433683896)-160.63638485582192)-17.757954917754752)-.799283237680523}(o)-.0098649429247001,o=1+o*function(r){return 0===r?30.33806074348246:30.33806074348246+r*(325.7925129965739+r*(1536.729586084437+r*(3199.8582195085955+r*(2553.0504064331644+r*(474.52854120695537+-22.44095244658582*r)))))}(o)}return i=kn(-(e=Bn(t,0))*e-.5625)*kn((e-t)*(e+t)+i/o),n?2-i/t:i/t}return n?2:0}var Rn=Math.sqrt;function Cn(r){return function(){return r}}function Yn(r){return jr(r)?Cn(NaN):function(n){if(jr(n))return NaN;return n<r?0:1}}function qn(r,n){var t;return jr(r)||jr(n)||n<0?Cn(NaN):0===n?Yn(r):(t=n*Rn(2),function(n){if(jr(n))return NaN;return.5*Wn(-(n-r)/t)})}function Xn(r){return s(r)===r&&r>0}function Dn(r){return r==r&&r>P&&r<V}l((function(r,n){return jr(r)||jr(n)?NaN:r<n?0:1}),"factory",Yn),l((function(r,n,t){return jr(r)||jr(n)||jr(t)||t<0?NaN:0===t?r<n?0:1:.5*Wn(-(r-n)/(t*Rn(2)))}),"factory",qn);var Qn,Jn=Math.round,Kn=.6931471805599453;function Zn(r){return r}Qn=function(r,n){var t,e;if(!cr(r))throw new TypeError("invalid argument. First argument must be a function. Value: `"+r+"`.");if(arguments.length<2)t=Zn;else if(!cr(t=n))throw new TypeError("invalid argument. Hash function argument must be a function. Value: `"+t+"`.");return l(i,"cache",e={}),i;function i(){var n,i,o,u;for(n=new Array(arguments.length),u=0;u<arguments.length;u++)n[u]=arguments[u];return o=t(n).toString(),w(e,o)?e[o]:(i=r.apply(null,n),e[o]=i,i)}}((function(r,n){var t;return 0===n?0===r?1:0:(t=n*(n+1)/2,r<0||r>t?0:(r>t/2&&(r=t-r),Qn(r-n,n-1)+Qn(r,n-1)))}));var $n=Qn;function rt(r,n){var t,e,i;if(jr(r)||!Xn(n)||!Dn(n))return NaN;if(r<0)return 0;if((r=Jn(r))>=n*(n+1)/2)return 1;for(t=kn(-n*Kn),i=0,e=0;e<=r;e++)i+=$n(e,n)*t;return i}function nt(r,n,t){var e,i;if(!hr(r)&&!gr(r))throw new TypeError("invalid argument. First argument must be an array-like object. Value: `"+r+"`.");if(0===(e=r.length))return-1;if(3===arguments.length){if(!M(t))throw new TypeError("invalid argument. `fromIndex` must be an integer. Value: `"+t+"`.");if(t>=0){if(t>=e)return-1;i=t}else(i=e+t)<0&&(i=0)}else i=0;if(Or(n)){for(;i<e;i++)if(Or(r[i]))return i}else for(;i<e;i++)if(r[i]===n)return i;return-1}function tt(r){var n,t,e,i,o,u,f;if(!hr(r))throw new TypeError("invalid argument. First argument must be a collection. Value: `"+r+"`.");for(n=0,t=[],i=[],e=r.length,u=0;u<e;u++)n+=1,-1===(f=nt(t,o=r[u]))?(t.push(o),i.push([o,1,0])):i[f][1]+=1;for(e=i.length,u=0;u<e;u++)i[u][2]=i[u][1]/n;return i}function et(r){return 0===r||jr(r)?r:r<0?-1:1}function it(){var r,n=arguments,t=n[0],e="https://stdlib.io/e/"+t+"?";for(r=1;r<n.length;r++)e+="&arg[]="+encodeURIComponent(n[r]);return e}l(rt,"factory",(function(r){var n,t;return Xn(r)&&Dn(r)?(t=kn(-r*Kn),n=r*(r+1)/2,function(e){var i,o;if(jr(e))return NaN;if(e<0)return 0;if((e=Jn(e))>=n)return 1;for(o=0,i=0;i<=e;i++)o+=$n(i,r)*t;return o}):Cn(NaN)}));var ot=["two-sided","less","greater"],ut=["pratt","wilcox","zsplit"];function ft(r,n){if(!yr(n))return new TypeError(it("0fs2h",n));if(w(n,"alpha")){if(r.alpha=n.alpha,!y(r.alpha)||Or(r.alpha))return new TypeError(it("0fs8h","alpha",r.alpha));if(r.alpha<0||r.alpha>1)return new RangeError(it("invalid option. `%s` option must be a number on the interval: [0, 1]. Option: `%f`.","alpha",r.alpha))}if(w(n,"alternative")){if(r.alternative=n.alternative,!gr(r.alternative))return new TypeError(it("0fs2i","alternative",r.alternative));if(!Tr(ot,r.alternative))return new Error(it("0fs3t","alternative",ot.join('", "'),r.alternative))}if(w(n,"correction")&&(r.correction=n.correction,!Y(r.correction)||Or(r.correction)))return new TypeError(it("0fs30","correction",r.alpha));if(w(n,"exact")&&(r.exact=n.exact,!Y(r.exact)||Or(r.exact)))return new TypeError(it("0fs30","exact",r.alpha));if(w(n,"mu")&&(r.mu=n.mu,!y(r.mu)||Or(r.mu)))return new TypeError(it("0fs8h","mu",r.mu));if(w(n,"zeroMethod")){if(r.zeroMethod=n.zeroMethod,!gr(r.zeroMethod))return new TypeError(it("0fs2i","zeroMethod",r.alternative));if(!Tr(ut,r.zeroMethod))return new Error(it("0fs3t","zeroMethod",ut.join('", "'),r.zeroMethod))}return null}function at(r,n){return r-n}function ct(r){var n,t,e,i;for((r=r.slice()).sort(at),n=r.length,e=1,i=0;e<n;e++)t=r[e],r[i]!==t&&(r[i+=1]=t);return r.length=i+1,r}function lt(r){return M(r)&&r>0}function st(r){return F(r)&&r.valueOf()>0}function pt(r){return lt(r)||st(r)}function vt(r){return p(r/2)}function yt(r){return vt(r>0?r-1:r+1)}function ht(r){return 0|r}l(pt,"isPrimitive",lt),l(pt,"isObject",st);var gt=!0===fn?1:0,dt=new qr(1),mt=new Gr(dt.buffer);function wt(r,n){return dt[0]=r,mt[gt]=n>>>0,dt[0]}var bt=1048576,jt=[1,1.5],Et=[0,.5849624872207642],At=[0,1.350039202129749e-8];var Ot=2147483647,Tt=1048575,Nt=1048576;var _t=2147483647,xt=1083179008,Ut=1e300,Vt=1e-300,Pt=[0,0],St=[0,0];function Mt(r,n){var t,e,i,o,u,f,a,c,l,s,v,y,h,g;if(jr(r)||jr(n))return NaN;if(yn(Pt,n),u=Pt[0],0===Pt[1]){if(0===n)return 1;if(1===n)return r;if(-1===n)return 1/r;if(.5===n)return Rn(r);if(-.5===n)return 1/Rn(r);if(2===n)return r*r;if(3===n)return r*r*r;if(4===n)return(r*=r)*r;if(kr(n))return function(r,n){return-1===r?(r-r)/(r-r):1===r?1:Un(r)<1==(n===V)?0:V}(r,n)}if(yn(Pt,r),o=Pt[0],0===Pt[1]){if(0===o)return function(r,n){return n===P?V:n===V?0:n>0?yt(n)?r:0:yt(n)?xn(V,r):V}(r,n);if(1===r)return 1;if(-1===r&&yt(n))return-1;if(kr(r))return r===P?Mt(-0,-n):n<0?0:V}if(r<0&&!1===p(n))return(r-r)/(r-r);if(i=Un(r),t=o&_t|0,e=u&_t|0,a=u>>>31|0,f=(f=o>>>31|0)&&yt(n)?-1:1,e>1105199104){if(e>1139802112)return function(r,n){return(2147483647&bn(r))<=1072693247?n<0?1/0:0:n>0?1/0:0}(r,n);if(t<1072693247)return 1===a?f*Ut*Ut:f*Vt*Vt;if(t>1072693248)return 0===a?f*Ut*Ut:f*Vt*Vt;v=function(r,n){var t,e,i,o,u,f;return t=(u=1.9259629911266175e-8*(i=n-1)-i*i*(0===(f=i)?.5:.5+f*(.25*f-.3333333333333333))*1.4426950408889634)-((e=Bn(e=(o=1.4426950216293335*i)+u,0))-o),r[0]=e,r[1]=t,r}(St,i)}else v=function(r,n,t){var e,i,o,u,f,a,c,l,s,p,v,y,h,g,d,m,w,b,j,E,A;return b=0,t<bt&&(b-=53,t=bn(n*=9007199254740992)),b+=(t>>20)-Ir|0,t=1072693248|(j=1048575&t|0),j<=235662?E=0:j<767610?E=1:(E=0,b+=1,t-=bt),u=Bn(i=(m=(n=wt(n,t))-(c=jt[E]))*(w=1/(n+c)),0),e=524288+(t>>1|536870912),a=wt(0,e+=E<<18),d=(o=i*i)*o*(0===(A=o)?.5999999999999946:.5999999999999946+A*(.4285714285785502+A*(.33333332981837743+A*(.272728123808534+A*(.23066074577556175+.20697501780033842*A))))),a=Bn(a=3+(o=u*u)+(d+=(f=w*(m-u*a-u*(n-(a-c))))*(u+i)),0),h=(v=-7.028461650952758e-9*(s=Bn(s=(m=u*a)+(w=f*a+(d-(a-3-o))*i),0))+.9617966939259756*(w-(s-m))+At[E])-((y=Bn(y=(p=.9617967009544373*s)+v+(l=Et[E])+(g=b),0))-g-l-p),r[0]=y,r[1]=h,r}(St,i,t);if(s=(n-(c=Bn(n,0)))*v[0]+n*v[1],l=c*v[0],yn(Pt,y=s+l),h=ht(Pt[0]),g=ht(Pt[1]),h>=xt){if(0!=(h-xt|g))return f*Ut*Ut;if(s+8008566259537294e-32>y-l)return f*Ut*Ut}else if((h&_t)>=1083231232){if(0!=(h-3230714880|g))return f*Vt*Vt;if(s<=y-l)return f*Vt*Vt}return y=function(r,n,t){var e,i,o,u,f,a,c,l,s,p;return s=((l=r&Ot|0)>>20)-Ir|0,c=0,l>1071644672&&(i=wt(0,((c=r+(Nt>>s+1)>>>0)&~(Tt>>(s=((c&Ot)>>20)-Ir|0)))>>>0),c=(c&Tt|Nt)>>20-s>>>0,r<0&&(c=-c),n-=i),r=ht(r=bn(a=1-((a=(o=.6931471824645996*(i=Bn(i=t+n,0)))+(u=(t-(i-n))*Kn+-1.904654299957768e-9*i))*(e=a-(i=a*a)*(0===(p=i)?.16666666666666602:.16666666666666602+p*(p*(6613756321437934e-20+p*(4.1381367970572385e-8*p-16533902205465252e-22))-.0027777777777015593)))/(e-2)-((f=u-(a-o))+a*f)-a))),(r+=c<<20>>>0)>>20<=0?Mn(a,c):wt(a,r)}(h,l,s),f*y}var Ft=1e308;function It(r,n){var t,e;return jr(r)||jr(n)||kr(n)?NaN:kr(r)||0===r||n<-324||Un(r)>9007199254740992&&n<=0?r:n>308?0*r:n<-308?(t=Mt(10,-(n+308)),kr(e=r*Ft*t)?r:Jn(e)/Ft/t):kr(e=r*(t=Mt(10,-n)))?r:Jn(e)/t}function kt(r){var n,t,e;if(t=4,n=!0,arguments.length>0){if(!yr(r))throw new TypeError(it("0fs3X",r));if(w(r,"digits")){if(!pt(r.digits))throw new TypeError(it("0fs3b","digits",r.digits));t=r.digits}if(w(r,"decision")){if(!Y(r.decision))throw new TypeError(it("0fs30","decision",r.decision));n=r.decision}}switch(e="",e+=this.method,e+="\n\n",e+="Alternative hypothesis: ","Paired Wilcoxon signed rank test"===this.method?e+="Median of the difference `x - y` is ":e+="Median of `x` is ",this.alternative){case"less":e+="less than ";break;case"greater":e+="greater than ";break;default:e+="not equal to "}return e+=this.nullValue,e+="\n\n",e+="    pValue: "+It(this.pValue,-t)+"\n",e+="    statistic: "+It(this.statistic,-t)+"\n",e+="\n",n&&(e+="Test Decision: ",this.rejected?e+="Reject null in favor of alternative at "+100*this.alpha+"% significance level":e+="Fail to reject null in favor of alternative at "+100*this.alpha+"% significance level",e+="\n"),e}var zt=qn(0,1);return function(){var r,n,t,e,i,o,u,f,a,c,l,s,p,v,y,h,g,d,m,w,b,j,E,A,O,T,N,x,U;if(!B(x=arguments[0])&&!_(x))throw new TypeError(it("0fs8j",x));if(h=x.length,arguments.length>1)if(yr(arguments[1]))t=arguments[1];else{if(!B(U=arguments[1])&&!_(U))throw new TypeError(it("invalid argument. `%s` argument must be a numeric array. Value: `%s`.","y",U));if(h!==U.length)throw new Error(it("0fs1H"));arguments.length>2&&(t=arguments[2])}if(s={},t&&(y=ft(s,t)))throw y;if(w=s.mu||0,r=void 0===s.correction||s.correction,c=void 0===s.alpha?.05:s.alpha,h<2)throw new Error(it("0fsAn",x));if(v=s.alternative||"two-sided","wilcox"===(n=s.zeroMethod||"wilcox")){if(E=[],U)for(A=0;A<h;A++)0!==(N=x[A]-U[A]-w)&&E.push(N);else for(A=0;A<h;A++)0!==x[A]&&E.push(x[A]-w);f=x.length-E.length}else if(E=new qr(h),f=0,U)for(A=0;A<h;A++)E[A]=x[A]-U[A]-w,0===E[A]&&(f+=1);else for(A=0;A<h;A++)E[A]=x[A]-w,0===E[A]&&(f+=1);if(f===h)throw new Error(it("0fs1Q"));for(h=E.length,m=new qr(h),A=0;A<h;A++)m[A]=Un(E[A]);for(O=Sr(m),u=0,a=0,A=0;A<h;A++)E[A]>0?u+=O[A]:0===E[A]&&(a+=O[A]);if(e=ct(O).length!==O.length,"zsplit"===n&&(u+=a/2),T=u,b=h*(h+1)*.25,j=h*(h+1)*(2*h+1),"pratt"===n){for(g=[],A=0;A<h;A++)0!==E[A]&&g.push(O[A]);O=g,b-=f*(f+1)*.25,j-=f*(f+1)*(2*f+1)}for(i=tt(O),o=0,A=0;A<i.length;A++)i[A][1]>1&&(o+=(N=i[A][1])*(N*N-1));if(o>0&&(j-=.5*o),j=Rn(j/24),h>50&&!s.exact||f>0||e){if(E=0,r)switch(v){case"two-sided":E=.5*et(T-b);break;case"less":E=-.5;break;default:E=.5}p=(T-b-E)/j,l="two-sided"===v?2*(1-zt(Un(p))):"greater"===v?1-zt(p):zt(p)}else p=T,l="two-sided"===v?p>h*(h+1)/4?2*(1-rt(p-1,h)):2*rt(p,h):"greater"===v?1-rt(p-1,h):rt(p,h);return G(d={},"rejected",l<=c),G(d,"alpha",c),G(d,"pValue",l),G(d,"statistic",T),G(d,"nullValue",w),G(d,"alternative",v),G(d,"method",(U?"Paired":"One-Sample")+" Wilcoxon signed rank test"),G(d,"print",kt),d}}));
//# sourceMappingURL=index.js.map

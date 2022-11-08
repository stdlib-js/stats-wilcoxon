// Copyright (c) 2022 The Stdlib Authors. License is Apache-2.0: http://www.apache.org/licenses/LICENSE-2.0
!function(r,n){"object"==typeof exports&&"undefined"!=typeof module?module.exports=n():"function"==typeof define&&define.amd?define(n):(r="undefined"!=typeof globalThis?globalThis:r||self).wilcoxon=n()}(this,(function(){"use strict";var r="function"==typeof Object.defineProperty?Object.defineProperty:null;var n,t=Object.defineProperty,e=Object.prototype,i=e.toString,o=e.__defineGetter__,u=e.__defineSetter__,a=e.__lookupGetter__,f=e.__lookupSetter__;n=function(){try{return r({},"x",{}),!0}catch(r){return!1}}()?t:function(r,n,t){var c,l,s,v;if("object"!=typeof r||null===r||"[object Array]"===i.call(r))throw new TypeError("invalid argument. First argument must be an object. Value: `"+r+"`.");if("object"!=typeof t||null===t||"[object Array]"===i.call(t))throw new TypeError("invalid argument. Property descriptor must be an object. Value: `"+t+"`.");if((l="value"in t)&&(a.call(r,n)||f.call(r,n)?(c=r.__proto__,r.__proto__=e,delete r[n],r[n]=t.value,r.__proto__=c):r[n]=t.value),s="get"in t,v="set"in t,l&&(s||v))throw new Error("invalid argument. Cannot specify one or more accessors and a value or writable attribute in the property descriptor.");return s&&o&&o.call(r,n,t.get),v&&u&&u.call(r,n,t.set),r};var c=n;function l(r,n,t){c(r,n,{configurable:!1,enumerable:!1,writable:!1,value:t})}var s=Math.floor;function v(r){return s(r)===r}function p(r){if("function"!=typeof r)throw new TypeError("invalid argument. Must provide a function. Value: `"+r+"`.");return function(n){var t,e;if(!function(r){return null!=r&&"function"!=typeof r&&"number"==typeof r.length&&v(r.length)&&r.length>=0&&r.length<=4294967295}(n))return!1;if(0===(t=n.length))return!1;for(e=0;e<t;e++)if(!1===r(n[e]))return!1;return!0}}function h(r){return"number"==typeof r}var y="function"==typeof Symbol&&"symbol"==typeof Symbol("foo");function g(){return y&&"symbol"==typeof Symbol.toStringTag}var d=Object.prototype.toString;var m=Object.prototype.hasOwnProperty;function b(r,n){return null!=r&&m.call(r,n)}var w="function"==typeof Symbol?Symbol.toStringTag:"";var j=g()?function(r){var n,t,e;if(null==r)return d.call(r);t=r[w],n=b(r,w);try{r[w]=void 0}catch(n){return d.call(r)}return e=d.call(r),n?r[w]=t:delete r[w],e}:function(r){return d.call(r)},E=Number,A=E.prototype.toString;var O=g();function T(r){return"object"==typeof r&&(r instanceof E||(O?function(r){try{return A.call(r),!0}catch(r){return!1}}(r):"[object Number]"===j(r)))}function N(r){return h(r)||T(r)}l(N,"isPrimitive",h),l(N,"isObject",T);var _=p(N.isPrimitive),x=p(N.isObject),U=p(N);l(U,"primitives",_),l(U,"objects",x);var V=Number.POSITIVE_INFINITY,P=E.NEGATIVE_INFINITY;function S(r){return r<V&&r>P&&v(r)}function M(r){return h(r)&&S(r)}function F(r){return T(r)&&S(r.valueOf())}function I(r){return M(r)||F(r)}function k(r){return M(r)&&r>=0}function z(r){return F(r)&&r.valueOf()>=0}function H(r){return k(r)||z(r)}l(I,"isPrimitive",M),l(I,"isObject",F),l(H,"isPrimitive",k),l(H,"isObject",z);var L=9007199254740991;function B(r){return null!==r&&"object"==typeof r&&k(r.length)&&r.length<=L&&"number"==typeof r.BYTES_PER_ELEMENT&&"number"==typeof r.byteOffset&&"number"==typeof r.byteLength}function G(r,n,t){c(r,n,{configurable:!1,enumerable:!0,writable:!1,value:t})}var W=Array.isArray?Array.isArray:function(r){return"[object Array]"===j(r)};function R(r){return"object"==typeof r&&null!==r&&!W(r)}var C=/./;function Y(r){return"boolean"==typeof r}var q=Boolean.prototype.toString;var X=g();function D(r){return"object"==typeof r&&(r instanceof Boolean||(X?function(r){try{return q.call(r),!0}catch(r){return!1}}(r):"[object Boolean]"===j(r)))}function Q(r){return Y(r)||D(r)}function J(){return new Function("return this;")()}l(Q,"isPrimitive",Y),l(Q,"isObject",D);var K="object"==typeof self?self:null,Z="object"==typeof window?window:null,$="object"==typeof global?global:null;var rr=function(r){if(arguments.length){if(!Y(r))throw new TypeError("invalid argument. Must provide a boolean primitive. Value: `"+r+"`.");if(r)return J()}if(K)return K;if(Z)return Z;if($)return $;throw new Error("unexpected error. Unable to resolve global object.")}(),nr=rr.document&&rr.document.childNodes,tr=Int8Array;function er(){return/^\s*function\s*([^(]*)/i}var ir=/^\s*function\s*([^(]*)/i;function or(r){return null!==r&&"object"==typeof r}function ur(r){var n,t,e,i;if(("Object"===(t=j(r).slice(8,-1))||"Error"===t)&&r.constructor){if("string"==typeof(e=r.constructor).name)return e.name;if(n=ir.exec(e.toString()))return n[1]}return or(i=r)&&(i._isBuffer||i.constructor&&"function"==typeof i.constructor.isBuffer&&i.constructor.isBuffer(i))?"Buffer":t}l(er,"REGEXP",ir),l(or,"isObjectLikeArray",function(r){if("function"!=typeof r)throw new TypeError("invalid argument. Must provide a function. Value: `"+r+"`.");return function(n){var t,e;if(!W(n))return!1;if(0===(t=n.length))return!1;for(e=0;e<t;e++)if(!1===r(n[e]))return!1;return!0}}(or));var ar="function"==typeof C||"object"==typeof tr||"function"==typeof nr?function(r){return ur(r).toLowerCase()}:function(r){var n;return null===r?"null":"object"===(n=typeof r)?ur(r).toLowerCase():n};function fr(r){return"function"===ar(r)}var cr,lr=Object.getPrototypeOf;cr=fr(Object.getPrototypeOf)?lr:function(r){var n=function(r){return r.__proto__}(r);return n||null===n?n:"[object Function]"===j(r.constructor)?r.constructor.prototype:r instanceof Object?Object.prototype:null};var sr=cr;var vr=Object.prototype;function pr(r){var n;return!!R(r)&&(n=function(r){return null==r?null:(r=Object(r),sr(r))}(r),!n||!b(r,"constructor")&&b(n,"constructor")&&fr(n.constructor)&&"[object Function]"===j(n.constructor)&&b(n,"isPrototypeOf")&&fr(n.isPrototypeOf)&&(n===vr||function(r){var n;for(n in r)if(!b(r,n))return!1;return!0}(r)))}function hr(r){return"object"==typeof r&&null!==r&&"number"==typeof r.length&&v(r.length)&&r.length>=0&&r.length<=L}function yr(r){return"string"==typeof r}var gr=String.prototype.valueOf;var dr=g();function mr(r){return"object"==typeof r&&(r instanceof String||(dr?function(r){try{return gr.call(r),!0}catch(r){return!1}}(r):"[object String]"===j(r)))}function br(r){return yr(r)||mr(r)}function wr(r){return r!=r}function jr(r){return h(r)&&wr(r)}function Er(r){return T(r)&&wr(r.valueOf())}function Ar(r){return jr(r)||Er(r)}function Or(r,n,t){var e,i,o;if(!hr(r)&&!yr(r))throw new TypeError("invalid argument. First argument must be array-like. Value: `"+r+"`.");if(arguments.length<2)throw new Error("insufficient input arguments. Must provide a search value.");if(arguments.length>2){if(!M(t))throw new TypeError("invalid argument. Third argument must be an integer. Value: `"+t+"`.");(i=t)<0&&(i=0)}else i=0;if(yr(r)){if(!yr(n))throw new TypeError("invalid argument. Second argument must be a string primitive. Value: `"+n+"`.");return-1!==r.indexOf(n,i)}if(e=r.length,jr(n)){for(o=i;o<e;o++)if(jr(r[o]))return!0;return!1}for(o=i;o<e;o++)if(r[o]===n)return!0;return!1}function Tr(r){var n,t,e;for(n=r.length,t=0,e=0;e<n;e++)t+=r[e];return t}function Nr(r){var n,t;for(n=new Array(r.length),t=0;t<r.length;t++)n[t]=t;return n.sort((function(n,t){return function(r,n){return r<n?-1:r>n?1:0}(r[n],r[t])}))}function _r(r,n){var t,e,i;for(t=r.length,e=new Array(t),i=0;i<t;i++)e[i]=Or(n,r[i]);return e}l(br,"isPrimitive",yr),l(br,"isObject",mr),l(Ar,"isPrimitive",jr),l(Ar,"isObject",Er);var xr=["min","max","average","dense","ordinal"],Ur=["last","first","remove"];function Vr(r,n){return R(n)?b(n,"encoding")&&(r.encoding=n.encoding,!W(r.encoding))?new TypeError("invalid option. `encoding` option must be an array. Option: `"+r.encoding+"`."):!b(n,"method")||(r.method=n.method,yr(r.method)&&Or(xr,r.method))?!b(n,"missing")||(r.missing=n.missing,yr(r.missing)&&Or(Ur,r.missing))?null:new TypeError("invalid option. `missing` must be one of the following values: `last`, `first`, or `remove`. Option: `"+r.missing+"`."):new TypeError("invalid option. `method` must be one of the following values: `average`, `min`, `max`, `dense`, or `ordinal`. Option: `"+r.method+"`."):new TypeError("invalid argument. Options argument must be an object. Value: `"+n+"`.")}function Pr(r,n){var t,e,i,o,u,a,f,c,l,s,v,p,h,y,g,d,m,b;if(!hr(r))throw new TypeError("invalid argument. First argument `x` must be an array-like object. Value: `"+r+"`.");if(h={},arguments.length>1&&(g=Vr(h,n)))throw g;for(v=h.method||"average",a=h.encoding||[null,NaN],l=h.missing||"last",d=r.length,y=[],m=0;m<d;m++)Or(a,r[m])||y.push(r[m]);if(t=_r(r,a),d=y.length,o=0,p=new Array(d),c=Nr(y),"ordinal"===v)for(m=0;m<d;m++)p[c[m]]=m+1;else for(e=0,m=0;m<d;m++)if(f=m+1,m===d-1||y[c[m]]!==y[c[f]]){switch(v){case"average":default:s=f-.5*e;break;case"min":s=f-e;break;case"max":s=f;break;case"dense":s=f-e-o,o+=e}for(b=m-e;b<f;b++)p[c[b]]=s;e=0}else e+=1;if("first"===l){for(i=Tr(t),b=1,u=new Array(t.length),m=0;m<t.length;m++)t[m]?(u[m]=b,b+=1):u[m]=p.shift()+i;return u}if("last"===l){for(u=new Array(t.length),m=0;m<t.length;m++)t[m]?u[m]=m+p.length+1:u[m]=p.shift();return u}return p}var Sr=Math.ceil;function Mr(r){return r<0?Sr(r):s(r)}var Fr=1023;function Ir(r){return r===V||r===P}var kr="function"==typeof Uint32Array;var zr="function"==typeof Uint32Array?Uint32Array:null;var Hr,Lr="function"==typeof Uint32Array?Uint32Array:void 0;Hr=function(){var r,n,t;if("function"!=typeof zr)return!1;try{n=new zr(n=[1,3.14,-3.14,4294967296,4294967297]),t=n,r=(kr&&t instanceof Uint32Array||"[object Uint32Array]"===j(t))&&1===n[0]&&3===n[1]&&4294967293===n[2]&&0===n[3]&&1===n[4]}catch(n){r=!1}return r}()?Lr:function(){throw new Error("not implemented")};var Br=Hr,Gr="function"==typeof Float64Array;var Wr="function"==typeof Float64Array?Float64Array:null;var Rr,Cr="function"==typeof Float64Array?Float64Array:void 0;Rr=function(){var r,n,t;if("function"!=typeof Wr)return!1;try{n=new Wr([1,3.14,-3.14,NaN]),t=n,r=(Gr&&t instanceof Float64Array||"[object Float64Array]"===j(t))&&1===n[0]&&3.14===n[1]&&-3.14===n[2]&&n[3]!=n[3]}catch(n){r=!1}return r}()?Cr:function(){throw new Error("not implemented")};var Yr=Rr,qr="function"==typeof Uint8Array;var Xr="function"==typeof Uint8Array?Uint8Array:null;var Dr,Qr="function"==typeof Uint8Array?Uint8Array:void 0;Dr=function(){var r,n,t;if("function"!=typeof Xr)return!1;try{n=new Xr(n=[1,3.14,-3.14,256,257]),t=n,r=(qr&&t instanceof Uint8Array||"[object Uint8Array]"===j(t))&&1===n[0]&&3===n[1]&&253===n[2]&&0===n[3]&&1===n[4]}catch(n){r=!1}return r}()?Qr:function(){throw new Error("not implemented")};var Jr=Dr,Kr="function"==typeof Uint16Array;var Zr="function"==typeof Uint16Array?Uint16Array:null;var $r,rn="function"==typeof Uint16Array?Uint16Array:void 0;$r=function(){var r,n,t;if("function"!=typeof Zr)return!1;try{n=new Zr(n=[1,3.14,-3.14,65536,65537]),t=n,r=(Kr&&t instanceof Uint16Array||"[object Uint16Array]"===j(t))&&1===n[0]&&3===n[1]&&65533===n[2]&&0===n[3]&&1===n[4]}catch(n){r=!1}return r}()?rn:function(){throw new Error("not implemented")};var nn,tn={uint16:$r,uint8:Jr};(nn=new tn.uint16(1))[0]=4660;var en,on,un=52===new tn.uint8(nn.buffer)[0];!0===un?(en=1,on=0):(en=0,on=1);var an={HIGH:en,LOW:on},fn=new Yr(1),cn=new Br(fn.buffer),ln=an.HIGH,sn=an.LOW;function vn(r,n){return fn[0]=n,r[0]=cn[ln],r[1]=cn[sn],r}function pn(r,n){return 1===arguments.length?vn([0,0],r):vn(r,n)}var hn,yn,gn=!0===un?1:0,dn=new Yr(1),mn=new Br(dn.buffer);function bn(r){return dn[0]=r,mn[gn]}!0===un?(hn=1,yn=0):(hn=0,yn=1);var wn={HIGH:hn,LOW:yn},jn=new Yr(1),En=new Br(jn.buffer),An=wn.HIGH,On=wn.LOW;function Tn(r,n){return En[An]=r,En[On]=n,jn[0]}var Nn=[0,0];function _n(r,n){var t,e;return pn(Nn,r),t=Nn[0],t&=2147483647,e=bn(n),Tn(t|=e&=2147483648,Nn[1])}function xn(r){return Math.abs(r)}function Un(r,n,t,e){return wr(r)||Ir(r)?(n[e]=r,n[e+t]=0,n):0!==r&&xn(r)<22250738585072014e-324?(n[e]=4503599627370496*r,n[e+t]=-52,n):(n[e]=r,n[e+t]=0,n)}l((function(r){return Un(r,[0,0],1,0)}),"assign",Un);var Vn=[0,0],Pn=[0,0];function Sn(r,n){var t,e;return 0===n||0===r||wr(r)||Ir(r)?r:(Un(r,Vn,1,0),n+=Vn[1],n+=function(r){var n=bn(r);return(n=(2146435072&n)>>>20)-Fr|0}(r=Vn[0]),n<-1074?_n(0,r):n>1023?r<0?P:V:(n<=-1023?(n+=52,e=2220446049250313e-31):e=1,pn(Pn,r),t=Pn[0],t&=2148532223,e*Tn(t|=n+Fr<<20,Pn[1])))}var Mn=1.4426950408889634,Fn=1/(1<<28);function In(r){var n;return wr(r)||r===V?r:r===P?0:r>709.782712893384?V:r<-745.1332191019411?0:r>-3.725290298461914e-9&&r<Fn?1+r:function(r,n,t){var e,i,o,u;return Sn(1-(n-(e=r-n)*(o=e-(i=e*e)*(0===(u=i)?.16666666666666602:.16666666666666602+u*(u*(6613756321437934e-20+u*(4.1381367970572385e-8*u-16533902205465252e-22))-.0027777777777015593)))/(2-o)-r),t)}(r-.6931471803691238*(n=Mr(r<0?Mn*r-.5:Mn*r+.5)),1.9082149292705877e-10*n,n)}var kn=!0===un?0:1,zn=new Yr(1),Hn=new Br(zn.buffer);function Ln(r,n){return zn[0]=r,Hn[kn]=n>>>0,zn[0]}var Bn=.8450629115104675;function Gn(r){var n,t,e,i,o,u,a,f;if(wr(r))return NaN;if(r===V)return 0;if(r===P)return 2;if(0===r)return 1;if(r<0?(n=!0,t=-r):(n=!1,t=r),t<.84375)return t<13877787807814457e-33?1-r:(i=.12837916709551256+(e=r*r)*function(r){return 0===r?-.3250421072470015:r*(r*(-23763016656650163e-21*r-.005770270296489442)-.02848174957559851)-.3250421072470015}(e),o=1+e*function(r){return 0===r?.39791722395915535:.39791722395915535+r*(.0650222499887673+r*(.005081306281875766+r*(.00013249473800432164+-3960228278775368e-21*r)))}(e),u=i/o,r<.25?1-(r+r*u):(i=r*u,.5-(i+=r-.5)));if(t<1.25)return a=(o=t-1)*function(r){return 0===r?.41485611868374833:.41485611868374833+r*(r*(.31834661990116175+r*(r*(.035478304325618236+-.002166375594868791*r)-.11089469428239668))-.3722078760357013)}(o)-.0023621185607526594,f=1+o*function(r){return 0===r?.10642088040084423:.10642088040084423+r*(.540397917702171+r*(.07182865441419627+r*(.12617121980876164+r*(.01363708391202905+.011984499846799107*r))))}(o),n?1+Bn+a/f:1-Bn-a/f;if(t<28){if(o=1/(t*t),t<2.857142857142857)i=o*function(r){return 0===r?-.6938585727071818:r*(r*(r*(r*(r*(-9.814329344169145*r-81.2874355063066)-184.60509290671104)-162.39666946257347)-62.375332450326006)-10.558626225323291)-.6938585727071818}(o)-.009864944034847148,o=1+o*function(r){return 0===r?19.651271667439257:19.651271667439257+r*(137.65775414351904+r*(434.56587747522923+r*(645.3872717332679+r*(429.00814002756783+r*(108.63500554177944+r*(6.570249770319282+-.0604244152148581*r))))))}(o);else{if(r<-6)return 2;i=o*function(r){return 0===r?-.799283237680523:r*(r*(r*(r*(-483.5191916086514*r-1025.0951316110772)-637.5664433683896)-160.63638485582192)-17.757954917754752)-.799283237680523}(o)-.0098649429247001,o=1+o*function(r){return 0===r?30.33806074348246:30.33806074348246+r*(325.7925129965739+r*(1536.729586084437+r*(3199.8582195085955+r*(2553.0504064331644+r*(474.52854120695537+-22.44095244658582*r)))))}(o)}return i=In(-(e=Ln(t,0))*e-.5625)*In((e-t)*(e+t)+i/o),n?2-i/t:i/t}return n?2:0}var Wn=Math.sqrt;function Rn(r){return function(){return r}}function Cn(r){return wr(r)?Rn(NaN):function(n){if(wr(n))return NaN;return n<r?0:1}}function Yn(r,n){var t;return wr(r)||wr(n)||n<0?Rn(NaN):0===n?Cn(r):(t=n*Wn(2),function(n){if(wr(n))return NaN;return.5*Gn(-(n-r)/t)})}function qn(r){return s(r)===r&&r>0}function Xn(r){return r==r&&r>P&&r<V}l((function(r,n){return wr(r)||wr(n)?NaN:r<n?0:1}),"factory",Cn),l((function(r,n,t){return wr(r)||wr(n)||wr(t)||t<0?NaN:0===t?r<n?0:1:.5*Gn(-(r-n)/(t*Wn(2)))}),"factory",Yn);var Dn,Qn=Math.round,Jn=.6931471805599453;function Kn(r){return r}Dn=function(r,n){var t,e;if(!fr(r))throw new TypeError("invalid argument. First argument must be a function. Value: `"+r+"`.");if(arguments.length<2)t=Kn;else if(!fr(t=n))throw new TypeError("invalid argument. Hash function argument must be a function. Value: `"+t+"`.");return l(i,"cache",e={}),i;function i(){var n,i,o,u;for(n=new Array(arguments.length),u=0;u<arguments.length;u++)n[u]=arguments[u];return o=t(n).toString(),b(e,o)?e[o]:(i=r.apply(null,n),e[o]=i,i)}}((function(r,n){var t;return 0===n?0===r?1:0:(t=n*(n+1)/2,r<0||r>t?0:(r>t/2&&(r=t-r),Dn(r-n,n-1)+Dn(r,n-1)))}));var Zn=Dn;function $n(r,n){var t,e,i;if(wr(r)||!qn(n)||!Xn(n))return NaN;if(r<0)return 0;if((r=Qn(r))>=n*(n+1)/2)return 1;for(t=In(-n*Jn),i=0,e=0;e<=r;e++)i+=Zn(e,n)*t;return i}function rt(r,n,t){var e,i;if(!hr(r)&&!yr(r))throw new TypeError("invalid argument. First argument must be an array-like object. Value: `"+r+"`.");if(0===(e=r.length))return-1;if(3===arguments.length){if(!M(t))throw new TypeError("invalid argument. `fromIndex` must be an integer. Value: `"+t+"`.");if(t>=0){if(t>=e)return-1;i=t}else(i=e+t)<0&&(i=0)}else i=0;if(Ar(n)){for(;i<e;i++)if(Ar(r[i]))return i}else for(;i<e;i++)if(r[i]===n)return i;return-1}function nt(r){var n,t,e,i,o,u,a;if(!hr(r))throw new TypeError("invalid argument. First argument must be a collection. Value: `"+r+"`.");for(n=0,t=[],i=[],e=r.length,u=0;u<e;u++)n+=1,-1===(a=rt(t,o=r[u]))?(t.push(o),i.push([o,1,0])):i[a][1]+=1;for(e=i.length,u=0;u<e;u++)i[u][2]=i[u][1]/n;return i}function tt(r){return 0===r||wr(r)?r:r<0?-1:1}function et(){var r,n=arguments,t=n[0],e="https://stdlib.io/e/"+t+"?";for(r=1;r<n.length;r++)e+="&arg[]="+encodeURIComponent(n[r]);return e}l($n,"factory",(function(r){var n,t;return qn(r)&&Xn(r)?(t=In(-r*Jn),n=r*(r+1)/2,function(e){var i,o;if(wr(e))return NaN;if(e<0)return 0;if((e=Qn(e))>=n)return 1;for(o=0,i=0;i<=e;i++)o+=Zn(i,r)*t;return o}):Rn(NaN)}));var it=["two-sided","less","greater"],ot=["pratt","wilcox","zsplit"];function ut(r,n){if(!pr(n))return new TypeError(et("0fs2h",n));if(b(n,"alpha")){if(r.alpha=n.alpha,!h(r.alpha)||Ar(r.alpha))return new TypeError(et("0fs8h","alpha",r.alpha));if(r.alpha<0||r.alpha>1)return new RangeError(et("invalid option. `%s` option must be a number on the interval: [0, 1]. Option: `%f`.","alpha",r.alpha))}if(b(n,"alternative")){if(r.alternative=n.alternative,!yr(r.alternative))return new TypeError(et("0fs2i","alternative",r.alternative));if(!Or(it,r.alternative))return new Error(et("0fs3t","alternative",it.join('", "'),r.alternative))}if(b(n,"correction")&&(r.correction=n.correction,!Y(r.correction)||Ar(r.correction)))return new TypeError(et("0fs30","correction",r.alpha));if(b(n,"exact")&&(r.exact=n.exact,!Y(r.exact)||Ar(r.exact)))return new TypeError(et("0fs30","exact",r.alpha));if(b(n,"mu")&&(r.mu=n.mu,!h(r.mu)||Ar(r.mu)))return new TypeError(et("0fs8h","mu",r.mu));if(b(n,"zeroMethod")){if(r.zeroMethod=n.zeroMethod,!yr(r.zeroMethod))return new TypeError(et("0fs2i","zeroMethod",r.alternative));if(!Or(ot,r.zeroMethod))return new Error(et("0fs3t","zeroMethod",ot.join('", "'),r.zeroMethod))}return null}function at(r,n){return r-n}function ft(r){var n,t,e,i;for((r=r.slice()).sort(at),n=r.length,e=1,i=0;e<n;e++)t=r[e],r[i]!==t&&(r[i+=1]=t);return r.length=i+1,r}function ct(r){return M(r)&&r>0}function lt(r){return F(r)&&r.valueOf()>0}function st(r){return ct(r)||lt(r)}function vt(r){return v(r/2)}function pt(r){return vt(r>0?r-1:r+1)}function ht(r){return 0|r}l(st,"isPrimitive",ct),l(st,"isObject",lt);var yt=!0===un?1:0,gt=new Yr(1),dt=new Br(gt.buffer);function mt(r,n){return gt[0]=r,dt[yt]=n>>>0,gt[0]}var bt=1048576,wt=[1,1.5],jt=[0,.5849624872207642],Et=[0,1.350039202129749e-8];var At=2147483647,Ot=1048575,Tt=1048576;var Nt=2147483647,_t=1083179008,xt=1e300,Ut=1e-300,Vt=[0,0],Pt=[0,0];function St(r,n){var t,e,i,o,u,a,f,c,l,s,p,h,y,g;if(wr(r)||wr(n))return NaN;if(pn(Vt,n),u=Vt[0],0===Vt[1]){if(0===n)return 1;if(1===n)return r;if(-1===n)return 1/r;if(.5===n)return Wn(r);if(-.5===n)return 1/Wn(r);if(2===n)return r*r;if(3===n)return r*r*r;if(4===n)return(r*=r)*r;if(Ir(n))return function(r,n){return-1===r?(r-r)/(r-r):1===r?1:xn(r)<1==(n===V)?0:V}(r,n)}if(pn(Vt,r),o=Vt[0],0===Vt[1]){if(0===o)return function(r,n){return n===P?V:n===V?0:n>0?pt(n)?r:0:pt(n)?_n(V,r):V}(r,n);if(1===r)return 1;if(-1===r&&pt(n))return-1;if(Ir(r))return r===P?St(-0,-n):n<0?0:V}if(r<0&&!1===v(n))return(r-r)/(r-r);if(i=xn(r),t=o&Nt|0,e=u&Nt|0,f=u>>>31|0,a=(a=o>>>31|0)&&pt(n)?-1:1,e>1105199104){if(e>1139802112)return function(r,n){return(2147483647&bn(r))<=1072693247?n<0?1/0:0:n>0?1/0:0}(r,n);if(t<1072693247)return 1===f?a*xt*xt:a*Ut*Ut;if(t>1072693248)return 0===f?a*xt*xt:a*Ut*Ut;p=function(r,n){var t,e,i,o,u,a;return t=(u=1.9259629911266175e-8*(i=n-1)-i*i*(0===(a=i)?.5:.5+a*(.25*a-.3333333333333333))*1.4426950408889634)-((e=Ln(e=(o=1.4426950216293335*i)+u,0))-o),r[0]=e,r[1]=t,r}(Pt,i)}else p=function(r,n,t){var e,i,o,u,a,f,c,l,s,v,p,h,y,g,d,m,b,w,j,E,A;return w=0,t<bt&&(w-=53,t=bn(n*=9007199254740992)),w+=(t>>20)-Fr|0,t=1072693248|(j=1048575&t|0),j<=235662?E=0:j<767610?E=1:(E=0,w+=1,t-=bt),u=Ln(i=(m=(n=mt(n,t))-(c=wt[E]))*(b=1/(n+c)),0),e=524288+(t>>1|536870912),f=mt(0,e+=E<<18),d=(o=i*i)*o*(0===(A=o)?.5999999999999946:.5999999999999946+A*(.4285714285785502+A*(.33333332981837743+A*(.272728123808534+A*(.23066074577556175+.20697501780033842*A))))),f=Ln(f=3+(o=u*u)+(d+=(a=b*(m-u*f-u*(n-(f-c))))*(u+i)),0),y=(p=-7.028461650952758e-9*(s=Ln(s=(m=u*f)+(b=a*f+(d-(f-3-o))*i),0))+.9617966939259756*(b-(s-m))+Et[E])-((h=Ln(h=(v=.9617967009544373*s)+p+(l=jt[E])+(g=w),0))-g-l-v),r[0]=h,r[1]=y,r}(Pt,i,t);if(s=(n-(c=Ln(n,0)))*p[0]+n*p[1],l=c*p[0],pn(Vt,h=s+l),y=ht(Vt[0]),g=ht(Vt[1]),y>=_t){if(0!=(y-_t|g))return a*xt*xt;if(s+8008566259537294e-32>h-l)return a*xt*xt}else if((y&Nt)>=1083231232){if(0!=(y-3230714880|g))return a*Ut*Ut;if(s<=h-l)return a*Ut*Ut}return h=function(r,n,t){var e,i,o,u,a,f,c,l,s,v;return s=((l=r&At|0)>>20)-Fr|0,c=0,l>1071644672&&(i=mt(0,((c=r+(Tt>>s+1)>>>0)&~(Ot>>(s=((c&At)>>20)-Fr|0)))>>>0),c=(c&Ot|Tt)>>20-s>>>0,r<0&&(c=-c),n-=i),r=ht(r=bn(f=1-((f=(o=.6931471824645996*(i=Ln(i=t+n,0)))+(u=(t-(i-n))*Jn+-1.904654299957768e-9*i))*(e=f-(i=f*f)*(0===(v=i)?.16666666666666602:.16666666666666602+v*(v*(6613756321437934e-20+v*(4.1381367970572385e-8*v-16533902205465252e-22))-.0027777777777015593)))/(e-2)-((a=u-(f-o))+f*a)-f))),(r+=c<<20>>>0)>>20<=0?Sn(f,c):mt(f,r)}(y,l,s),a*h}var Mt=1e308;function Ft(r,n){var t,e;return wr(r)||wr(n)||Ir(n)?NaN:Ir(r)||0===r||n<-324||xn(r)>9007199254740992&&n<=0?r:n>308?0*r:n<-308?(t=St(10,-(n+308)),Ir(e=r*Mt*t)?r:Qn(e)/Mt/t):Ir(e=r*(t=St(10,-n)))?r:Qn(e)/t}function It(r){var n,t,e;if(t=4,n=!0,arguments.length>0){if(!pr(r))throw new TypeError(et("0fs3X",r));if(b(r,"digits")){if(!st(r.digits))throw new TypeError(et("0fs3b","digits",r.digits));t=r.digits}if(b(r,"decision")){if(!Y(r.decision))throw new TypeError(et("0fs30","decision",r.decision));n=r.decision}}switch(e="",e+=this.method,e+="\n\n",e+="Alternative hypothesis: ","Paired Wilcoxon signed rank test"===this.method?e+="Median of the difference `x - y` is ":e+="Median of `x` is ",this.alternative){case"less":e+="less than ";break;case"greater":e+="greater than ";break;default:e+="not equal to "}return e+=this.nullValue,e+="\n\n",e+="    pValue: "+Ft(this.pValue,-t)+"\n",e+="    statistic: "+Ft(this.statistic,-t)+"\n",e+="\n",n&&(e+="Test Decision: ",this.rejected?e+="Reject null in favor of alternative at "+100*this.alpha+"% significance level":e+="Fail to reject null in favor of alternative at "+100*this.alpha+"% significance level",e+="\n"),e}var kt=Yn(0,1);return function(){var r,n,t,e,i,o,u,a,f,c,l,s,v,p,h,y,g,d,m,b,w,j,E,A,O,T,N,x,U;if(!B(x=arguments[0])&&!_(x))throw new TypeError(et("0fs8j",x));if(y=x.length,arguments.length>1)if(pr(arguments[1]))t=arguments[1];else{if(!B(U=arguments[1])&&!_(U))throw new TypeError(et("invalid argument. `%s` argument must be a numeric array. Value: `%s`.","y",U));if(y!==U.length)throw new Error(et("0fs1H"));arguments.length>2&&(t=arguments[2])}if(s={},t&&(h=ut(s,t)))throw h;if(b=s.mu||0,r=void 0===s.correction||s.correction,c=void 0===s.alpha?.05:s.alpha,y<2)throw new Error(et("0fsAn",x));if(p=s.alternative||"two-sided","wilcox"===(n=s.zeroMethod||"wilcox")){if(E=[],U)for(A=0;A<y;A++)0!==(N=x[A]-U[A]-b)&&E.push(N);else for(A=0;A<y;A++)0!==x[A]&&E.push(x[A]-b);a=x.length-E.length}else if(E=new Yr(y),a=0,U)for(A=0;A<y;A++)E[A]=x[A]-U[A]-b,0===E[A]&&(a+=1);else for(A=0;A<y;A++)E[A]=x[A]-b,0===E[A]&&(a+=1);if(a===y)throw new Error(et("0fs1Q"));for(y=E.length,m=new Yr(y),A=0;A<y;A++)m[A]=xn(E[A]);for(O=Pr(m),u=0,f=0,A=0;A<y;A++)E[A]>0?u+=O[A]:0===E[A]&&(f+=O[A]);if(e=ft(O).length!==O.length,"zsplit"===n&&(u+=f/2),T=u,w=y*(y+1)*.25,j=y*(y+1)*(2*y+1),"pratt"===n){for(g=[],A=0;A<y;A++)0!==E[A]&&g.push(O[A]);O=g,w-=a*(a+1)*.25,j-=a*(a+1)*(2*a+1)}for(i=nt(O),o=0,A=0;A<i.length;A++)i[A][1]>1&&(o+=(N=i[A][1])*(N*N-1));if(o>0&&(j-=.5*o),j=Wn(j/24),y>50&&!s.exact||a>0||e){if(E=0,r)switch(p){case"two-sided":E=.5*tt(T-w);break;case"less":E=-.5;break;default:E=.5}v=(T-w-E)/j,l="two-sided"===p?2*(1-kt(xn(v))):"greater"===p?1-kt(v):kt(v)}else v=T,l="two-sided"===p?v>y*(y+1)/4?2*(1-$n(v-1,y)):2*$n(v,y):"greater"===p?1-$n(v-1,y):$n(v,y);return G(d={},"rejected",l<=c),G(d,"alpha",c),G(d,"pValue",l),G(d,"statistic",T),G(d,"nullValue",b),G(d,"alternative",p),G(d,"method",(U?"Paired":"One-Sample")+" Wilcoxon signed rank test"),G(d,"print",It),d}}));
//# sourceMappingURL=index.js.map

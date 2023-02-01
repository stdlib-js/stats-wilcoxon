// Copyright (c) 2023 The Stdlib Authors. License is Apache-2.0: http://www.apache.org/licenses/LICENSE-2.0
/// <reference types="./mod.d.ts" />
var r="function"==typeof Object.defineProperty?Object.defineProperty:null;var n,t=Object.defineProperty,e=Object.prototype,i=e.toString,o=e.__defineGetter__,u=e.__defineSetter__,a=e.__lookupGetter__,f=e.__lookupSetter__;n=function(){try{return r({},"x",{}),!0}catch(r){return!1}}()?t:function(r,n,t){var c,l,s,p;if("object"!=typeof r||null===r||"[object Array]"===i.call(r))throw new TypeError("invalid argument. First argument must be an object. Value: `"+r+"`.");if("object"!=typeof t||null===t||"[object Array]"===i.call(t))throw new TypeError("invalid argument. Property descriptor must be an object. Value: `"+t+"`.");if((l="value"in t)&&(a.call(r,n)||f.call(r,n)?(c=r.__proto__,r.__proto__=e,delete r[n],r[n]=t.value,r.__proto__=c):r[n]=t.value),s="get"in t,p="set"in t,l&&(s||p))throw new Error("invalid argument. Cannot specify one or more accessors and a value or writable attribute in the property descriptor.");return s&&o&&o.call(r,n,t.get),p&&u&&u.call(r,n,t.set),r};var c=n;function l(r,n,t){c(r,n,{configurable:!1,enumerable:!1,writable:!1,value:t})}var s=Math.floor;function p(r){return s(r)===r}function v(r){if("function"!=typeof r)throw new TypeError("invalid argument. Must provide a function. Value: `"+r+"`.");return function(n){var t,e;if(!function(r){return null!=r&&"function"!=typeof r&&"number"==typeof r.length&&p(r.length)&&r.length>=0&&r.length<=4294967295}(n))return!1;if(0===(t=n.length))return!1;for(e=0;e<t;e++)if(!1===r(n[e]))return!1;return!0}}function y(r){return"number"==typeof r}var h="function"==typeof Symbol&&"symbol"==typeof Symbol("foo");function g(){return h&&"symbol"==typeof Symbol.toStringTag}var d=Object.prototype.toString;var m=Object.prototype.hasOwnProperty;function w(r,n){return null!=r&&m.call(r,n)}var b="function"==typeof Symbol?Symbol.toStringTag:"";var j=g()?function(r){var n,t,e;if(null==r)return d.call(r);t=r[b],n=w(r,b);try{r[b]=void 0}catch(n){return d.call(r)}return e=d.call(r),n?r[b]=t:delete r[b],e}:function(r){return d.call(r)},E=Number,A=E.prototype.toString;var O=g();function T(r){return"object"==typeof r&&(r instanceof E||(O?function(r){try{return A.call(r),!0}catch(r){return!1}}(r):"[object Number]"===j(r)))}function N(r){return y(r)||T(r)}l(N,"isPrimitive",y),l(N,"isObject",T);var _=v(N.isPrimitive),x=v(N.isObject),U=v(N);l(U,"primitives",_),l(U,"objects",x);var V=Number.POSITIVE_INFINITY,P=E.NEGATIVE_INFINITY;function S(r){return r<V&&r>P&&p(r)}function M(r){return y(r)&&S(r)}function F(r){return T(r)&&S(r.valueOf())}function I(r){return M(r)||F(r)}function k(r){return M(r)&&r>=0}function z(r){return F(r)&&r.valueOf()>=0}function H(r){return k(r)||z(r)}l(I,"isPrimitive",M),l(I,"isObject",F),l(H,"isPrimitive",k),l(H,"isObject",z);function L(r){return null!==r&&"object"==typeof r&&k(r.length)&&r.length<=9007199254740991&&"number"==typeof r.BYTES_PER_ELEMENT&&"number"==typeof r.byteOffset&&"number"==typeof r.byteLength}function B(r,n,t){c(r,n,{configurable:!1,enumerable:!0,writable:!1,value:t})}var G=Array.isArray?Array.isArray:function(r){return"[object Array]"===j(r)};function W(r){return"object"==typeof r&&null!==r&&!G(r)}var R=/./;function C(r){return"boolean"==typeof r}var Y=Boolean.prototype.toString;var q=g();function X(r){return"object"==typeof r&&(r instanceof Boolean||(q?function(r){try{return Y.call(r),!0}catch(r){return!1}}(r):"[object Boolean]"===j(r)))}function D(r){return C(r)||X(r)}function Q(){return new Function("return this;")()}l(D,"isPrimitive",C),l(D,"isObject",X);var J="object"==typeof self?self:null,K="object"==typeof window?window:null,Z="undefined"!=typeof global?global:"undefined"!=typeof self?self:"undefined"!=typeof window?window:{},$="object"==typeof Z?Z:null;var rr=function(r){if(arguments.length){if(!C(r))throw new TypeError("invalid argument. Must provide a boolean primitive. Value: `"+r+"`.");if(r)return Q()}if(J)return J;if(K)return K;if($)return $;throw new Error("unexpected error. Unable to resolve global object.")}(),nr=rr.document&&rr.document.childNodes,tr=Int8Array;function er(){return/^\s*function\s*([^(]*)/i}var ir=/^\s*function\s*([^(]*)/i;function or(r){return null!==r&&"object"==typeof r}function ur(r){var n,t,e,i;if(("Object"===(t=j(r).slice(8,-1))||"Error"===t)&&r.constructor){if("string"==typeof(e=r.constructor).name)return e.name;if(n=ir.exec(e.toString()))return n[1]}return or(i=r)&&(i._isBuffer||i.constructor&&"function"==typeof i.constructor.isBuffer&&i.constructor.isBuffer(i))?"Buffer":t}l(er,"REGEXP",ir),l(or,"isObjectLikeArray",function(r){if("function"!=typeof r)throw new TypeError("invalid argument. Must provide a function. Value: `"+r+"`.");return function(n){var t,e;if(!G(n))return!1;if(0===(t=n.length))return!1;for(e=0;e<t;e++)if(!1===r(n[e]))return!1;return!0}}(or));var ar="function"==typeof R||"object"==typeof tr||"function"==typeof nr?function(r){return ur(r).toLowerCase()}:function(r){var n;return null===r?"null":"object"===(n=typeof r)?ur(r).toLowerCase():n};function fr(r){return"function"===ar(r)}var cr,lr=Object.getPrototypeOf;cr=fr(Object.getPrototypeOf)?lr:function(r){var n=function(r){return r.__proto__}(r);return n||null===n?n:"[object Function]"===j(r.constructor)?r.constructor.prototype:r instanceof Object?Object.prototype:null};var sr=cr;var pr=Object.prototype;function vr(r){var n;return!!W(r)&&(n=function(r){return null==r?null:(r=Object(r),sr(r))}(r),!n||!w(r,"constructor")&&w(n,"constructor")&&fr(n.constructor)&&"[object Function]"===j(n.constructor)&&w(n,"isPrototypeOf")&&fr(n.isPrototypeOf)&&(n===pr||function(r){var n;for(n in r)if(!w(r,n))return!1;return!0}(r)))}function yr(r){return"object"==typeof r&&null!==r&&"number"==typeof r.length&&p(r.length)&&r.length>=0&&r.length<=9007199254740991}function hr(r){return"string"==typeof r}var gr=String.prototype.valueOf;var dr=g();function mr(r){return"object"==typeof r&&(r instanceof String||(dr?function(r){try{return gr.call(r),!0}catch(r){return!1}}(r):"[object String]"===j(r)))}function wr(r){return hr(r)||mr(r)}function br(r){return r!=r}function jr(r){return y(r)&&br(r)}function Er(r){return T(r)&&br(r.valueOf())}function Ar(r){return jr(r)||Er(r)}function Or(r,n,t){var e,i,o;if(!yr(r)&&!hr(r))throw new TypeError("invalid argument. First argument must be array-like. Value: `"+r+"`.");if(arguments.length<2)throw new Error("insufficient input arguments. Must provide a search value.");if(arguments.length>2){if(!M(t))throw new TypeError("invalid argument. Third argument must be an integer. Value: `"+t+"`.");(i=t)<0&&(i=0)}else i=0;if(hr(r)){if(!hr(n))throw new TypeError("invalid argument. Second argument must be a string primitive. Value: `"+n+"`.");return-1!==r.indexOf(n,i)}if(e=r.length,jr(n)){for(o=i;o<e;o++)if(jr(r[o]))return!0;return!1}for(o=i;o<e;o++)if(r[o]===n)return!0;return!1}function Tr(r){var n,t,e;for(n=r.length,t=0,e=0;e<n;e++)t+=r[e];return t}function Nr(r){var n,t;for(n=new Array(r.length),t=0;t<r.length;t++)n[t]=t;return n.sort((function(n,t){return function(r,n){return r<n?-1:r>n?1:0}(r[n],r[t])}))}function _r(r,n){var t,e,i;for(t=r.length,e=new Array(t),i=0;i<t;i++)e[i]=Or(n,r[i]);return e}l(wr,"isPrimitive",hr),l(wr,"isObject",mr),l(Ar,"isPrimitive",jr),l(Ar,"isObject",Er);var xr=["min","max","average","dense","ordinal"],Ur=["last","first","remove"];function Vr(r,n){return W(n)?w(n,"encoding")&&(r.encoding=n.encoding,!G(r.encoding))?new TypeError("invalid option. `encoding` option must be an array. Option: `"+r.encoding+"`."):!w(n,"method")||(r.method=n.method,hr(r.method)&&Or(xr,r.method))?!w(n,"missing")||(r.missing=n.missing,hr(r.missing)&&Or(Ur,r.missing))?null:new TypeError("invalid option. `missing` must be one of the following values: `last`, `first`, or `remove`. Option: `"+r.missing+"`."):new TypeError("invalid option. `method` must be one of the following values: `average`, `min`, `max`, `dense`, or `ordinal`. Option: `"+r.method+"`."):new TypeError("invalid argument. Options argument must be an object. Value: `"+n+"`.")}function Pr(r,n){var t,e,i,o,u,a,f,c,l,s,p,v,y,h,g,d,m,w;if(!yr(r))throw new TypeError("invalid argument. First argument `x` must be an array-like object. Value: `"+r+"`.");if(y={},arguments.length>1&&(g=Vr(y,n)))throw g;for(p=y.method||"average",a=y.encoding||[null,NaN],l=y.missing||"last",d=r.length,h=[],m=0;m<d;m++)Or(a,r[m])||h.push(r[m]);if(t=_r(r,a),d=h.length,o=0,v=new Array(d),c=Nr(h),"ordinal"===p)for(m=0;m<d;m++)v[c[m]]=m+1;else for(e=0,m=0;m<d;m++)if(f=m+1,m===d-1||h[c[m]]!==h[c[f]]){switch(p){case"average":default:s=f-.5*e;break;case"min":s=f-e;break;case"max":s=f;break;case"dense":s=f-e-o,o+=e}for(w=m-e;w<f;w++)v[c[w]]=s;e=0}else e+=1;if("first"===l){for(i=Tr(t),w=1,u=new Array(t.length),m=0;m<t.length;m++)t[m]?(u[m]=w,w+=1):u[m]=v.shift()+i;return u}if("last"===l){for(u=new Array(t.length),m=0;m<t.length;m++)t[m]?u[m]=m+v.length+1:u[m]=v.shift();return u}return v}var Sr=Math.ceil;function Mr(r){return r<0?Sr(r):s(r)}function Fr(r){return r===V||r===P}var Ir="function"==typeof Uint32Array;var kr="function"==typeof Uint32Array?Uint32Array:null;var zr,Hr="function"==typeof Uint32Array?Uint32Array:void 0;zr=function(){var r,n,t;if("function"!=typeof kr)return!1;try{n=new kr(n=[1,3.14,-3.14,4294967296,4294967297]),t=n,r=(Ir&&t instanceof Uint32Array||"[object Uint32Array]"===j(t))&&1===n[0]&&3===n[1]&&4294967293===n[2]&&0===n[3]&&1===n[4]}catch(n){r=!1}return r}()?Hr:function(){throw new Error("not implemented")};var Lr=zr,Br="function"==typeof Float64Array;var Gr="function"==typeof Float64Array?Float64Array:null;var Wr,Rr="function"==typeof Float64Array?Float64Array:void 0;Wr=function(){var r,n,t;if("function"!=typeof Gr)return!1;try{n=new Gr([1,3.14,-3.14,NaN]),t=n,r=(Br&&t instanceof Float64Array||"[object Float64Array]"===j(t))&&1===n[0]&&3.14===n[1]&&-3.14===n[2]&&n[3]!=n[3]}catch(n){r=!1}return r}()?Rr:function(){throw new Error("not implemented")};var Cr=Wr,Yr="function"==typeof Uint8Array;var qr="function"==typeof Uint8Array?Uint8Array:null;var Xr,Dr="function"==typeof Uint8Array?Uint8Array:void 0;Xr=function(){var r,n,t;if("function"!=typeof qr)return!1;try{n=new qr(n=[1,3.14,-3.14,256,257]),t=n,r=(Yr&&t instanceof Uint8Array||"[object Uint8Array]"===j(t))&&1===n[0]&&3===n[1]&&253===n[2]&&0===n[3]&&1===n[4]}catch(n){r=!1}return r}()?Dr:function(){throw new Error("not implemented")};var Qr=Xr,Jr="function"==typeof Uint16Array;var Kr="function"==typeof Uint16Array?Uint16Array:null;var Zr,$r="function"==typeof Uint16Array?Uint16Array:void 0;Zr=function(){var r,n,t;if("function"!=typeof Kr)return!1;try{n=new Kr(n=[1,3.14,-3.14,65536,65537]),t=n,r=(Jr&&t instanceof Uint16Array||"[object Uint16Array]"===j(t))&&1===n[0]&&3===n[1]&&65533===n[2]&&0===n[3]&&1===n[4]}catch(n){r=!1}return r}()?$r:function(){throw new Error("not implemented")};var rn,nn={uint16:Zr,uint8:Qr};(rn=new nn.uint16(1))[0]=4660;var tn,en,on=52===new nn.uint8(rn.buffer)[0];!0===on?(tn=1,en=0):(tn=0,en=1);var un={HIGH:tn,LOW:en},an=new Cr(1),fn=new Lr(an.buffer),cn=un.HIGH,ln=un.LOW;function sn(r,n,t,e){return an[0]=r,n[e]=fn[cn],n[e+t]=fn[ln],n}function pn(r){return sn(r,[0,0],1,0)}l(pn,"assign",sn);var vn,yn,hn=!0===on?1:0,gn=new Cr(1),dn=new Lr(gn.buffer);function mn(r){return gn[0]=r,dn[hn]}!0===on?(vn=1,yn=0):(vn=0,yn=1);var wn={HIGH:vn,LOW:yn},bn=new Cr(1),jn=new Lr(bn.buffer),En=wn.HIGH,An=wn.LOW;function On(r,n){return jn[En]=r,jn[An]=n,bn[0]}var Tn=[0,0];function Nn(r,n){var t,e;return pn.assign(r,Tn,1,0),t=Tn[0],t&=2147483647,e=mn(n),On(t|=e&=2147483648,Tn[1])}function _n(r){return Math.abs(r)}function xn(r,n,t,e){return br(r)||Fr(r)?(n[e]=r,n[e+t]=0,n):0!==r&&_n(r)<22250738585072014e-324?(n[e]=4503599627370496*r,n[e+t]=-52,n):(n[e]=r,n[e+t]=0,n)}l((function(r){return xn(r,[0,0],1,0)}),"assign",xn);var Un=[0,0],Vn=[0,0];function Pn(r,n){var t,e;return 0===n||0===r||br(r)||Fr(r)?r:(xn(r,Un,1,0),n+=Un[1],n+=function(r){var n=mn(r);return(n=(2146435072&n)>>>20)-1023|0}(r=Un[0]),n<-1074?Nn(0,r):n>1023?r<0?P:V:(n<=-1023?(n+=52,e=2220446049250313e-31):e=1,pn.assign(r,Vn,1,0),t=Vn[0],t&=2148532223,e*On(t|=n+1023<<20,Vn[1])))}function Sn(r){var n;return br(r)||r===V?r:r===P?0:r>709.782712893384?V:r<-745.1332191019411?0:r>-3.725290298461914e-9&&r<3.725290298461914e-9?1+r:function(r,n,t){var e,i,o,u;return Pn(1-(n-(e=r-n)*(o=e-(i=e*e)*(0===(u=i)?.16666666666666602:.16666666666666602+u*(u*(6613756321437934e-20+u*(4.1381367970572385e-8*u-16533902205465252e-22))-.0027777777777015593)))/(2-o)-r),t)}(r-.6931471803691238*(n=Mr(r<0?1.4426950408889634*r-.5:1.4426950408889634*r+.5)),1.9082149292705877e-10*n,n)}var Mn=!0===on?0:1,Fn=new Cr(1),In=new Lr(Fn.buffer);function kn(r,n){return Fn[0]=r,In[Mn]=n>>>0,Fn[0]}var zn=.8450629115104675;function Hn(r){var n,t,e,i,o,u,a,f;if(br(r))return NaN;if(r===V)return 0;if(r===P)return 2;if(0===r)return 1;if(r<0?(n=!0,t=-r):(n=!1,t=r),t<.84375)return t<13877787807814457e-33?1-r:(i=.12837916709551256+(e=r*r)*function(r){return 0===r?-.3250421072470015:r*(r*(-23763016656650163e-21*r-.005770270296489442)-.02848174957559851)-.3250421072470015}(e),o=1+e*function(r){return 0===r?.39791722395915535:.39791722395915535+r*(.0650222499887673+r*(.005081306281875766+r*(.00013249473800432164+-3960228278775368e-21*r)))}(e),u=i/o,r<.25?1-(r+r*u):(i=r*u,.5-(i+=r-.5)));if(t<1.25)return a=(o=t-1)*function(r){return 0===r?.41485611868374833:.41485611868374833+r*(r*(.31834661990116175+r*(r*(.035478304325618236+-.002166375594868791*r)-.11089469428239668))-.3722078760357013)}(o)-.0023621185607526594,f=1+o*function(r){return 0===r?.10642088040084423:.10642088040084423+r*(.540397917702171+r*(.07182865441419627+r*(.12617121980876164+r*(.01363708391202905+.011984499846799107*r))))}(o),n?1+zn+a/f:1-zn-a/f;if(t<28){if(o=1/(t*t),t<2.857142857142857)i=o*function(r){return 0===r?-.6938585727071818:r*(r*(r*(r*(r*(-9.814329344169145*r-81.2874355063066)-184.60509290671104)-162.39666946257347)-62.375332450326006)-10.558626225323291)-.6938585727071818}(o)-.009864944034847148,o=1+o*function(r){return 0===r?19.651271667439257:19.651271667439257+r*(137.65775414351904+r*(434.56587747522923+r*(645.3872717332679+r*(429.00814002756783+r*(108.63500554177944+r*(6.570249770319282+-.0604244152148581*r))))))}(o);else{if(r<-6)return 2;i=o*function(r){return 0===r?-.799283237680523:r*(r*(r*(r*(-483.5191916086514*r-1025.0951316110772)-637.5664433683896)-160.63638485582192)-17.757954917754752)-.799283237680523}(o)-.0098649429247001,o=1+o*function(r){return 0===r?30.33806074348246:30.33806074348246+r*(325.7925129965739+r*(1536.729586084437+r*(3199.8582195085955+r*(2553.0504064331644+r*(474.52854120695537+-22.44095244658582*r)))))}(o)}return i=Sn(-(e=kn(t,0))*e-.5625)*Sn((e-t)*(e+t)+i/o),n?2-i/t:i/t}return n?2:0}var Ln=Math.sqrt;function Bn(r){return function(){return r}}function Gn(r){return br(r)?Bn(NaN):function(n){if(br(n))return NaN;return n<r?0:1}}function Wn(r,n){var t;return br(r)||br(n)||n<0?Bn(NaN):0===n?Gn(r):(t=n*Ln(2),function(n){if(br(n))return NaN;return.5*Hn(-(n-r)/t)})}function Rn(r){return s(r)===r&&r>0}function Cn(r){return r==r&&r>P&&r<V}l((function(r,n){return br(r)||br(n)?NaN:r<n?0:1}),"factory",Gn),l((function(r,n,t){return br(r)||br(n)||br(t)||t<0?NaN:0===t?r<n?0:1:.5*Hn(-(r-n)/(t*Ln(2)))}),"factory",Wn);var Yn,qn=Math.round,Xn=.6931471805599453;function Dn(r){return r}var Qn=Yn=function(r,n){var t,e;if(!fr(r))throw new TypeError("invalid argument. First argument must be a function. Value: `"+r+"`.");if(arguments.length<2)t=Dn;else if(!fr(t=n))throw new TypeError("invalid argument. Hash function argument must be a function. Value: `"+t+"`.");return l(i,"cache",e={}),i;function i(){var n,i,o,u;for(n=new Array(arguments.length),u=0;u<arguments.length;u++)n[u]=arguments[u];return o=t(n).toString(),w(e,o)?e[o]:(i=r.apply(null,n),e[o]=i,i)}}((function(r,n){var t;return 0===n?0===r?1:0:(t=n*(n+1)/2,r<0||r>t?0:(r>t/2&&(r=t-r),Yn(r-n,n-1)+Yn(r,n-1)))}));function Jn(r,n){var t,e,i;if(br(r)||!Rn(n)||!Cn(n))return NaN;if(r<0)return 0;if((r=qn(r))>=n*(n+1)/2)return 1;for(t=Sn(-n*Xn),i=0,e=0;e<=r;e++)i+=Qn(e,n)*t;return i}function Kn(r,n,t){var e,i;if(!yr(r)&&!hr(r))throw new TypeError("invalid argument. First argument must be an array-like object. Value: `"+r+"`.");if(0===(e=r.length))return-1;if(3===arguments.length){if(!M(t))throw new TypeError("invalid argument. `fromIndex` must be an integer. Value: `"+t+"`.");if(t>=0){if(t>=e)return-1;i=t}else(i=e+t)<0&&(i=0)}else i=0;if(Ar(n)){for(;i<e;i++)if(Ar(r[i]))return i}else for(;i<e;i++)if(r[i]===n)return i;return-1}function Zn(r){var n,t,e,i,o,u,a;if(!yr(r))throw new TypeError("invalid argument. First argument must be a collection. Value: `"+r+"`.");for(n=0,t=[],i=[],e=r.length,u=0;u<e;u++)n+=1,-1===(a=Kn(t,o=r[u]))?(t.push(o),i.push([o,1,0])):i[a][1]+=1;for(e=i.length,u=0;u<e;u++)i[u][2]=i[u][1]/n;return i}function $n(r){return 0===r||br(r)?r:r<0?-1:1}function rt(){var r,n=arguments,t=n[0],e="https://stdlib.io/e/"+t+"?";for(r=1;r<n.length;r++)e+="&arg[]="+encodeURIComponent(n[r]);return e}l(Jn,"factory",(function(r){var n,t;return Rn(r)&&Cn(r)?(t=Sn(-r*Xn),n=r*(r+1)/2,function(e){var i,o;if(br(e))return NaN;if(e<0)return 0;if((e=qn(e))>=n)return 1;for(o=0,i=0;i<=e;i++)o+=Qn(i,r)*t;return o}):Bn(NaN)}));var nt=["two-sided","less","greater"],tt=["pratt","wilcox","zsplit"];function et(r,n){if(!vr(n))return new TypeError(rt("0fs2h",n));if(w(n,"alpha")){if(r.alpha=n.alpha,!y(r.alpha)||Ar(r.alpha))return new TypeError(rt("0fs8h","alpha",r.alpha));if(r.alpha<0||r.alpha>1)return new RangeError(rt("invalid option. `%s` option must be a number on the interval: [0, 1]. Option: `%f`.","alpha",r.alpha))}if(w(n,"alternative")){if(r.alternative=n.alternative,!hr(r.alternative))return new TypeError(rt("0fs2i","alternative",r.alternative));if(!Or(nt,r.alternative))return new Error(rt("0fs3t","alternative",nt.join('", "'),r.alternative))}if(w(n,"correction")&&(r.correction=n.correction,!C(r.correction)||Ar(r.correction)))return new TypeError(rt("0fs30","correction",r.alpha));if(w(n,"exact")&&(r.exact=n.exact,!C(r.exact)||Ar(r.exact)))return new TypeError(rt("0fs30","exact",r.alpha));if(w(n,"mu")&&(r.mu=n.mu,!y(r.mu)||Ar(r.mu)))return new TypeError(rt("0fs8h","mu",r.mu));if(w(n,"zeroMethod")){if(r.zeroMethod=n.zeroMethod,!hr(r.zeroMethod))return new TypeError(rt("0fs2i","zeroMethod",r.alternative));if(!Or(tt,r.zeroMethod))return new Error(rt("0fs3t","zeroMethod",tt.join('", "'),r.zeroMethod))}return null}function it(r,n){return r-n}function ot(r){var n,t,e,i;for((r=r.slice()).sort(it),n=r.length,e=1,i=0;e<n;e++)t=r[e],r[i]!==t&&(r[i+=1]=t);return r.length=i+1,r}function ut(r){return M(r)&&r>0}function at(r){return F(r)&&r.valueOf()>0}function ft(r){return ut(r)||at(r)}function ct(r){return p(r/2)}function lt(r){return ct(r>0?r-1:r+1)}function st(r){return 0|r}l(ft,"isPrimitive",ut),l(ft,"isObject",at);var pt=!0===on?1:0,vt=new Cr(1),yt=new Lr(vt.buffer);function ht(r,n){return vt[0]=r,yt[pt]=n>>>0,vt[0]}var gt=[1,1.5],dt=[0,.5849624872207642],mt=[0,1.350039202129749e-8];var wt=1e300,bt=1e-300,jt=[0,0],Et=[0,0];function At(r,n){var t,e,i,o,u,a,f,c,l,s,v,y,h,g;if(br(r)||br(n))return NaN;if(pn.assign(n,jt,1,0),u=jt[0],0===jt[1]){if(0===n)return 1;if(1===n)return r;if(-1===n)return 1/r;if(.5===n)return Ln(r);if(-.5===n)return 1/Ln(r);if(2===n)return r*r;if(3===n)return r*r*r;if(4===n)return(r*=r)*r;if(Fr(n))return function(r,n){return-1===r?(r-r)/(r-r):1===r?1:_n(r)<1==(n===V)?0:V}(r,n)}if(pn.assign(r,jt,1,0),o=jt[0],0===jt[1]){if(0===o)return function(r,n){return n===P?V:n===V?0:n>0?lt(n)?r:0:lt(n)?Nn(V,r):V}(r,n);if(1===r)return 1;if(-1===r&&lt(n))return-1;if(Fr(r))return r===P?At(-0,-n):n<0?0:V}if(r<0&&!1===p(n))return(r-r)/(r-r);if(i=_n(r),t=2147483647&o|0,e=2147483647&u|0,f=u>>>31|0,a=(a=o>>>31|0)&&lt(n)?-1:1,e>1105199104){if(e>1139802112)return function(r,n){return(2147483647&mn(r))<=1072693247?n<0?1/0:0:n>0?1/0:0}(r,n);if(t<1072693247)return 1===f?a*wt*wt:a*bt*bt;if(t>1072693248)return 0===f?a*wt*wt:a*bt*bt;v=function(r,n){var t,e,i,o,u,a;return t=(u=1.9259629911266175e-8*(i=n-1)-i*i*(0===(a=i)?.5:.5+a*(.25*a-.3333333333333333))*1.4426950408889634)-((e=kn(e=(o=1.4426950216293335*i)+u,0))-o),r[0]=e,r[1]=t,r}(Et,i)}else v=function(r,n,t){var e,i,o,u,a,f,c,l,s,p,v,y,h,g,d,m,w,b,j,E,A;return b=0,t<1048576&&(b-=53,t=mn(n*=9007199254740992)),b+=(t>>20)-1023|0,t=1072693248|(j=1048575&t|0),j<=235662?E=0:j<767610?E=1:(E=0,b+=1,t-=1048576),u=kn(i=(m=(n=ht(n,t))-(c=gt[E]))*(w=1/(n+c)),0),e=524288+(t>>1|536870912),f=ht(0,e+=E<<18),d=(o=i*i)*o*(0===(A=o)?.5999999999999946:.5999999999999946+A*(.4285714285785502+A*(.33333332981837743+A*(.272728123808534+A*(.23066074577556175+.20697501780033842*A))))),f=kn(f=3+(o=u*u)+(d+=(a=w*(m-u*f-u*(n-(f-c))))*(u+i)),0),h=(v=-7.028461650952758e-9*(s=kn(s=(m=u*f)+(w=a*f+(d-(f-3-o))*i),0))+.9617966939259756*(w-(s-m))+mt[E])-((y=kn(y=(p=.9617967009544373*s)+v+(l=dt[E])+(g=b),0))-g-l-p),r[0]=y,r[1]=h,r}(Et,i,t);if(y=(s=(n-(c=kn(n,0)))*v[0]+n*v[1])+(l=c*v[0]),pn.assign(y,jt,1,0),h=st(jt[0]),g=st(jt[1]),h>=1083179008){if(0!=(h-1083179008|g))return a*wt*wt;if(s+8008566259537294e-32>y-l)return a*wt*wt}else if((2147483647&h)>=1083231232){if(0!=(h-3230714880|g))return a*bt*bt;if(s<=y-l)return a*bt*bt}return y=function(r,n,t){var e,i,o,u,a,f,c,l,s,p,v;return p=((s=2147483647&r|0)>>20)-1023|0,l=0,s>1071644672&&(e=((l=r+(1048576>>p+1)>>>0)&~(1048575>>(p=((2147483647&l)>>20)-1023|0)))>>>0,l=(1048575&l|1048576)>>20-p>>>0,r<0&&(l=-l),n-=o=ht(0,e)),r=st(r=mn(c=1-((c=(u=.6931471824645996*(o=kn(o=t+n,0)))+(a=(t-(o-n))*Xn+-1.904654299957768e-9*o))*(i=c-(o=c*c)*(0===(v=o)?.16666666666666602:.16666666666666602+v*(v*(6613756321437934e-20+v*(4.1381367970572385e-8*v-16533902205465252e-22))-.0027777777777015593)))/(i-2)-((f=a-(c-u))+c*f)-c))),(r+=l<<20>>>0)>>20<=0?Pn(c,l):ht(c,r)}(h,l,s),a*y}var Ot=1e308;function Tt(r,n){var t,e;return br(r)||br(n)||Fr(n)?NaN:Fr(r)||0===r||n<-324||_n(r)>9007199254740992&&n<=0?r:n>308?0*r:n<-308?(t=At(10,-(n+308)),Fr(e=r*Ot*t)?r:qn(e)/Ot/t):Fr(e=r*(t=At(10,-n)))?r:qn(e)/t}function Nt(r){var n,t,e;if(t=4,n=!0,arguments.length>0){if(!vr(r))throw new TypeError(rt("0fs3X",r));if(w(r,"digits")){if(!ft(r.digits))throw new TypeError(rt("0fs3b","digits",r.digits));t=r.digits}if(w(r,"decision")){if(!C(r.decision))throw new TypeError(rt("0fs30","decision",r.decision));n=r.decision}}switch(e="",e+=this.method,e+="\n\n",e+="Alternative hypothesis: ","Paired Wilcoxon signed rank test"===this.method?e+="Median of the difference `x - y` is ":e+="Median of `x` is ",this.alternative){case"less":e+="less than ";break;case"greater":e+="greater than ";break;default:e+="not equal to "}return e+=this.nullValue,e+="\n\n",e+="    pValue: "+Tt(this.pValue,-t)+"\n",e+="    statistic: "+Tt(this.statistic,-t)+"\n",e+="\n",n&&(e+="Test Decision: ",this.rejected?e+="Reject null in favor of alternative at "+100*this.alpha+"% significance level":e+="Fail to reject null in favor of alternative at "+100*this.alpha+"% significance level",e+="\n"),e}var _t=Wn(0,1);function xt(){var r,n,t,e,i,o,u,a,f,c,l,s,p,v,y,h,g,d,m,w,b,j,E,A,O,T,N,x,U;if(!L(x=arguments[0])&&!_(x))throw new TypeError(rt("0fs8j",x));if(h=x.length,arguments.length>1)if(vr(arguments[1]))t=arguments[1];else{if(!L(U=arguments[1])&&!_(U))throw new TypeError(rt("invalid argument. `%s` argument must be a numeric array. Value: `%s`.","y",U));if(h!==U.length)throw new Error(rt("0fs1H"));arguments.length>2&&(t=arguments[2])}if(s={},t&&(y=et(s,t)))throw y;if(w=s.mu||0,r=void 0===s.correction||s.correction,c=void 0===s.alpha?.05:s.alpha,h<2)throw new Error(rt("0fsAn",x));if(v=s.alternative||"two-sided","wilcox"===(n=s.zeroMethod||"wilcox")){if(E=[],U)for(A=0;A<h;A++)0!==(N=x[A]-U[A]-w)&&E.push(N);else for(A=0;A<h;A++)0!==x[A]&&E.push(x[A]-w);a=x.length-E.length}else if(E=new Cr(h),a=0,U)for(A=0;A<h;A++)E[A]=x[A]-U[A]-w,0===E[A]&&(a+=1);else for(A=0;A<h;A++)E[A]=x[A]-w,0===E[A]&&(a+=1);if(a===h)throw new Error(rt("0fs1Q"));for(h=E.length,m=new Cr(h),A=0;A<h;A++)m[A]=_n(E[A]);for(O=Pr(m),u=0,f=0,A=0;A<h;A++)E[A]>0?u+=O[A]:0===E[A]&&(f+=O[A]);if(e=ot(O).length!==O.length,"zsplit"===n&&(u+=f/2),T=u,b=h*(h+1)*.25,j=h*(h+1)*(2*h+1),"pratt"===n){for(g=[],A=0;A<h;A++)0!==E[A]&&g.push(O[A]);O=g,b-=a*(a+1)*.25,j-=a*(a+1)*(2*a+1)}for(i=Zn(O),o=0,A=0;A<i.length;A++)i[A][1]>1&&(o+=(N=i[A][1])*(N*N-1));if(o>0&&(j-=.5*o),j=Ln(j/24),h>50&&!s.exact||a>0||e){if(E=0,r)switch(v){case"two-sided":E=.5*$n(T-b);break;case"less":E=-.5;break;default:E=.5}p=(T-b-E)/j,l="two-sided"===v?2*(1-_t(_n(p))):"greater"===v?1-_t(p):_t(p)}else p=T,l="two-sided"===v?p>h*(h+1)/4?2*(1-Jn(p-1,h)):2*Jn(p,h):"greater"===v?1-Jn(p-1,h):Jn(p,h);return B(d={},"rejected",l<=c),B(d,"alpha",c),B(d,"pValue",l),B(d,"statistic",T),B(d,"nullValue",w),B(d,"alternative",v),B(d,"method",(U?"Paired":"One-Sample")+" Wilcoxon signed rank test"),B(d,"print",Nt),d}export{xt as default};
//# sourceMappingURL=mod.js.map

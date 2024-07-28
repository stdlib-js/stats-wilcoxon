"use strict";var V=function(e,i){return function(){return i||e((i={exports:{}}).exports,i),i.exports}};var H=V(function(Oe,W){"use strict";var N=require("@stdlib/assert-contains"),_=require("@stdlib/assert-is-boolean").isPrimitive,B=require("@stdlib/assert-is-number").isPrimitive,te=require("@stdlib/assert-is-plain-object"),C=require("@stdlib/assert-is-string").isPrimitive,j=require("@stdlib/assert-is-nan"),E=require("@stdlib/assert-has-own-property"),d=require("@stdlib/string-format"),I=["two-sided","less","greater"],U=["pratt","wilcox","zsplit"];function ne(e,i){if(!te(i))return new TypeError(d("invalid argument. Options argument must be an object. Value: `%s`.",i));if(E(i,"alpha")){if(e.alpha=i.alpha,!B(e.alpha)||j(e.alpha))return new TypeError(d("invalid option. `%s` option must be a number. Option: `%s`.","alpha",e.alpha));if(e.alpha<0||e.alpha>1)return new RangeError(d("invalid option. `%s` option must be a number on the interval: [0, 1]. Option: `%f`.","alpha",e.alpha))}if(E(i,"alternative")){if(e.alternative=i.alternative,!C(e.alternative))return new TypeError(d("invalid option. `%s` option must be a string. Option: `%s`.","alternative",e.alternative));if(!N(I,e.alternative))return new Error(d('invalid option. `%s` option must be one of the following: "%s". Option: `%s`.',"alternative",I.join('", "'),e.alternative))}if(E(i,"correction")&&(e.correction=i.correction,!_(e.correction)||j(e.correction)))return new TypeError(d("invalid option. `%s` option must be a boolean. Option: `%s`.","correction",e.alpha));if(E(i,"exact")&&(e.exact=i.exact,!_(e.exact)||j(e.exact)))return new TypeError(d("invalid option. `%s` option must be a boolean. Option: `%s`.","exact",e.alpha));if(E(i,"mu")&&(e.mu=i.mu,!B(e.mu)||j(e.mu)))return new TypeError(d("invalid option. `%s` option must be a number. Option: `%s`.","mu",e.mu));if(E(i,"zeroMethod")){if(e.zeroMethod=i.zeroMethod,!C(e.zeroMethod))return new TypeError(d("invalid option. `%s` option must be a string. Option: `%s`.","zeroMethod",e.alternative));if(!N(U,e.zeroMethod))return new Error(d('invalid option. `%s` option must be one of the following: "%s". Option: `%s`.',"zeroMethod",U.join('", "'),e.zeroMethod))}return null}W.exports=ne});var G=V(function(Te,Z){"use strict";function oe(e,i){return e-i}function se(e){var i,o,a,u;for(e=e.slice(),e.sort(oe),i=e.length,a=1,u=0;a<i;a++)o=e[a],e[u]!==o&&(u+=1,e[u]=o);return e.length=u+1,e}Z.exports=se});var X=V(function(ze,Q){"use strict";var le=require("@stdlib/assert-is-positive-integer"),ue=require("@stdlib/assert-is-plain-object"),ve=require("@stdlib/assert-is-boolean").isPrimitive,J=require("@stdlib/assert-has-own-property"),K=require("@stdlib/math-base-special-roundn"),D=require("@stdlib/string-format");function fe(e){var i,o,a;if(o=4,i=!0,arguments.length>0){if(!ue(e))throw new TypeError(D("invalid argument. First argument must be an object. Value: `%s`.",e));if(J(e,"digits")){if(!le(e.digits))throw new TypeError(D("invalid option. `%s` option must be a positive integer. Option: `%s`.","digits",e.digits));o=e.digits}if(J(e,"decision")){if(!ve(e.decision))throw new TypeError(D("invalid option. `%s` option must be a boolean. Option: `%s`.","decision",e.decision));i=e.decision}}switch(a="",a+=this.method,a+="\n\n",a+="Alternative hypothesis: ",this.method==="Paired Wilcoxon signed rank test"?a+="Median of the difference `x - y` is ":a+="Median of `x` is ",this.alternative){case"less":a+="less than ";break;case"greater":a+="greater than ";break;case"two-sided":default:a+="not equal to ";break}return a+=this.nullValue,a+="\n\n",a+="    pValue: "+K(this.pValue,-o)+"\n",a+="    statistic: "+K(this.statistic,-o)+"\n",a+="\n",i&&(a+="Test Decision: ",this.rejected?a+="Reject null in favor of alternative at "+this.alpha*100+"% significance level":a+="Fail to reject null in favor of alternative at "+this.alpha*100+"% significance level",a+="\n"),a}Q.exports=fe});var ae=V(function(Me,ie){"use strict";var Y=require("@stdlib/assert-is-number-array").primitives,$=require("@stdlib/assert-is-typed-array-like"),p=require("@stdlib/utils-define-read-only-property"),ce=require("@stdlib/assert-is-plain-object"),he=require("@stdlib/stats-ranks"),de=require("@stdlib/stats-base-dists-normal-cdf").factory,k=require("@stdlib/stats-base-dists-signrank-cdf"),me=require("@stdlib/utils-tabulate"),ge=require("@stdlib/math-base-special-signum"),pe=require("@stdlib/math-base-special-sqrt"),ee=require("@stdlib/math-base-special-abs"),re=require("@stdlib/array-float64"),L=require("@stdlib/string-format"),we=H(),be=G(),qe=X(),S=de(0,1);function ye(){var e,i,o,a,u,O,T,l,P,z,m,v,f,w,A,t,F,c,R,q,M,y,n,r,g,x,b,s,h;if(s=arguments[0],!$(s)&&!Y(s))throw new TypeError(L("invalid argument. First argument must be a numeric array. Value: `%s`.",s));if(t=s.length,arguments.length>1)if(ce(arguments[1]))o=arguments[1];else{if(h=arguments[1],!$(h)&&!Y(h))throw new TypeError(L("invalid argument. `%s` argument must be a numeric array. Value: `%s`.","y",h));if(t!==h.length)throw new Error("invalid arguments. First and second arguments must have the same length.");arguments.length>2&&(o=arguments[2])}if(v={},o&&(A=we(v,o),A))throw A;if(q=v.mu||0,v.correction===void 0?e=!0:e=v.correction,v.alpha===void 0?z=.05:z=v.alpha,t<2)throw new Error(L("invalid argument. First argument must contain at least two elements. Value: `%s`.",s));if(w=v.alternative||"two-sided",i=v.zeroMethod||"wilcox",i==="wilcox"){if(n=[],h)for(r=0;r<t;r++)b=s[r]-h[r]-q,b!==0&&n.push(b);else for(r=0;r<t;r++)s[r]!==0&&n.push(s[r]-q);l=s.length-n.length}else if(n=new re(t),l=0,h)for(r=0;r<t;r++)n[r]=s[r]-h[r]-q,n[r]===0&&(l+=1);else for(r=0;r<t;r++)n[r]=s[r]-q,n[r]===0&&(l+=1);if(l===t)throw new Error("`x` or `x - y` cannot be zero for all elements.");for(t=n.length,R=new re(t),r=0;r<t;r++)R[r]=ee(n[r]);for(g=he(R),T=0,P=0,r=0;r<t;r++)n[r]>0?T+=g[r]:n[r]===0&&(P+=g[r]);if(a=be(g).length!==g.length,i==="zsplit"&&(T+=P/2),x=T,M=t*(t+1)*.25,y=t*(t+1)*(2*t+1),i==="pratt"){for(F=[],r=0;r<t;r++)n[r]!==0&&F.push(g[r]);g=F,M-=l*(l+1)*.25,y-=l*(l+1)*(2*l+1)}for(u=me(g),O=0,r=0;r<u.length;r++)u[r][1]>1&&(b=u[r][1],O+=b*(b*b-1));if(O>0&&(y-=.5*O),y=pe(y/24),t>50&&!v.exact||l>0||a){if(n=0,e)switch(w){case"two-sided":n=.5*ge(x-M);break;case"less":n=-.5;break;default:n=.5;break}f=(x-M-n)/y,w==="two-sided"?m=2*(1-S(ee(f))):w==="greater"?m=1-S(f):m=S(f)}else f=x,w==="two-sided"?f>t*(t+1)/4?m=2*(1-k(f-1,t)):m=2*k(f,t):w==="greater"?m=1-k(f-1,t):m=k(f,t);return c={},p(c,"rejected",m<=z),p(c,"alpha",z),p(c,"pValue",m),p(c,"statistic",x),p(c,"nullValue",q),p(c,"alternative",w),p(c,"method",(h?"Paired":"One-Sample")+" Wilcoxon signed rank test"),p(c,"print",qe),c}ie.exports=ye});var Ee=ae();module.exports=Ee;
/**
* @license Apache-2.0
*
* Copyright (c) 2020 The Stdlib Authors.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
//# sourceMappingURL=index.js.map

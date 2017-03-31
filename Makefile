clean:
	rm -f *.pvl *.pv
secrecy-1-ab-oneway:
	make clean
	./ps2pv/jsdefp.native --pv ps/sp.js > secrecy-1-ab-oneway.pv
	cat tl/secrecy-1-ab-oneway.pv >> secrecy-1-ab-oneway.pv
secrecy-1-ab-twoway:
	make clean
	./ps2pv/jsdefp.native --pv ps/sp.js > secrecy-1-ab-twoway.pv
	cat tl/secrecy-1-ab-twoway.pv >> secrecy-1-ab-twoway.pv
authenticity-1-ab-oneway:
	make clean
	./ps2pv/jsdefp.native --pv ps/sp.js > authenticity-1-ab-oneway.pv
	cat tl/authenticity-1-ab-oneway.pv >> authenticity-1-ab-oneway.pv
authenticity-1-ab-twoway:
	make clean
	./ps2pv/jsdefp.native --pv ps/sp.js > authenticity-1-ab-twoway.pv
	cat tl/authenticity-1-ab-twoway.pv >> authenticity-1-ab-twoway.pv
authenticity-1-abm-oneway:
	make clean
	./ps2pv/jsdefp.native --pv ps/sp.js > authenticity-1-abm-oneway.pv
	cat tl/authenticity-1-abm-oneway.pv >> authenticity-1-abm-oneway.pv
authenticity-1-abm-twoway:
	make clean
	./ps2pv/jsdefp.native --pv ps/sp.js > authenticity-1-abm-twoway.pv
	cat tl/authenticity-1-abm-twoway.pv >> authenticity-1-abm-twoway.pv
forwardsecrecy-1-ab-oneway:
	make clean
	./ps2pv/jsdefp.native --pv ps/sp.js > forwardsecrecy-1-ab-oneway.pv
	cat tl/forwardsecrecy-1-ab-oneway.pv >> forwardsecrecy-1-ab-oneway.pv
forwardsecrecy-2-ab-oneway:
	make clean
	./ps2pv/jsdefp.native --pv ps/sp.js > forwardsecrecy-2-ab-oneway.pv
	cat tl/forwardsecrecy-2-ab-oneway.pv >> forwardsecrecy-2-ab-oneway.pv
forwardsecrecy-3-ab-oneway:
	make clean
	./ps2pv/jsdefp.native --pv ps/sp.js > forwardsecrecy-3-ab-oneway.pv
	cat tl/forwardsecrecy-3-ab-oneway.pv >> forwardsecrecy-3-ab-oneway.pv
futuresecrecy-3-ab-oneway:
	make clean
	./ps2pv/jsdefp.native --pv ps/sp.js > futuresecrecy-3-ab-oneway.pv
	cat tl/futuresecrecy-3-ab-oneway.pv >> futuresecrecy-3-ab-oneway.pv
kci-1-a-oneway:
	make clean
	./ps2pv/jsdefp.native --pv ps/sp.js > kci-1-a-oneway.pv
	cat tl/kci-1-a-oneway.pv >> kci-1-a-oneway.pv
kci-1-b-oneway:
	make clean
	./ps2pv/jsdefp.native --pv ps/sp.js > kci-1-b-oneway.pv
	cat tl/kci-1-b-oneway.pv >> kci-1-b-oneway.pv
indistinguishability-1-ab-oneway:
	make clean
	./ps2pv/jsdefp.native --pv ps/sp.js > indistinguishability-1-ab-oneway.pv
	cat tl/indistinguishability-1-ab-oneway.pv >> indistinguishability-1-ab-oneway.pv

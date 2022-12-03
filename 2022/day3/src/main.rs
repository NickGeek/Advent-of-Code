use std::collections::HashSet;

fn main() {
    p1(INPUT_T);
    println!("\n---------------\nfinal answer:");
    p1(INPUT);
    println!("\n---------------\np2:");
    p2(INPUT_T);
    println!("\n---------------\nfinal answer:");
    p2(INPUT);
}

fn p1(input: &str) {
    let mut a = input.lines()
        .map(|items| {
            let sack = items.chars()
                .map(|item| {
                    if item.is_ascii_uppercase() {
                        ((item as u8) - b'A') + 27
                    } else {
                        ((item as u8) - b'a') + 1
                    }
                })
                .collect::<Vec<_>>();
            let res: Vec<Vec<u8>> = sack.chunks(sack.len() / 2).map(|s|s.into()).collect();
            (res[0].clone(),res[1].clone())
        }
        )
        .collect::<Vec<_>>();

    let res: u64 = a.iter()
        .map(|(c1,c2)| {
            let c1s = HashSet::<&u8>::from_iter(c1.iter());
            let c2s = HashSet::<&u8>::from_iter(c2.iter());
            let common = **c1s.intersection(&c2s).next().unwrap();
            common as u64
        })
        .sum();

    println!("{:?}", res);
}

fn p2(input: &str) {
    let mut a = input.lines()
        .map(|items| {
            items.chars()
                .map(|item| {
                    if item.is_ascii_uppercase() {
                        ((item as u8) - b'A') + 27
                    } else {
                        ((item as u8) - b'a') + 1
                    }
                })
                .collect::<Vec<_>>()
            // let res: Vec<Vec<u8>> = sack.chunks(sack.len() / 2).map(|s|s.into()).collect();
            // (res[0].clone(),res[1].clone())
        }
        )
        .collect::<Vec<_>>();

    let res: u64 = a.chunks(3)
        .map(|elves| {
            let mut hs: HashSet<u8> = HashSet::new();
            let c1s = HashSet::<&u8>::from_iter(elves[0].iter());
            let c2s = HashSet::<&u8>::from_iter(elves[1].iter());
            let c3s = HashSet::<&u8>::from_iter(elves[2].iter());
            let hs1 = c1s.intersection(&c2s).copied().collect::<HashSet<_>>();
            let common = *hs1.intersection(&c3s).next().unwrap();
            *common as u64
        })
        .sum();

    println!("{:?}", res);
}

const INPUT_T: &str = "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw";

const INPUT: &str = "DPstqDdrsqdDtqrFDJDDrmtsJHflSJCLgCphgHHgRHJCRRff
BcBGcQzVBVZcvznTTTvZcGTpCRRRfRCggLflHlhhCZpZCj
vGQnQvnzTzNTTbVnzGBqMqwqDLdPtMmbwqqLLM
wLRFRqvFsFRjfrHddbdbjzdH
lcsnSJPSSVVlGmGrHzbbrGNrdzbz
mSmlnnPlmJmncVDSlSZSlmLBCvtwBvtLCqqswsDBCTWW
pfqPrPgmmhvqdlsdWq
nfjHLJfZcLbVtQWWtndhls
CzJJFLzRzfDwrmggpC
CWfllmlCDFlZZqMfmFBWmWLJVRLVwNNtRVGPpwtGpqbJ
jHndndndcjhscnhHNtRbVtLbGpJbRRcb
HSrvnQzQSMDlLzBCfg
BQRVbgQQBJBbBtVBSSSRWMQbdNvvRPjZjCCdPLNZNNsNCCzd
HwpFpnlGpGZWGvjzPd
FTDmFrrwDpFMtmQVQQcWgc
VhbPshVDPDFWhWsgDNMMbVtmBjwBffpwBntnmnqfnswt
QzzGrTZZdrdlTcCLpRBnmBRRjBCqtptt
rJdGlmLTJdJrvZDbSbDSWDNbFJgD
qrcqTBHTcHgwWWdHRjdWBglBbGPpGvvPbszGzsbPpQfPLwPz
nFVmjhMjFJCSJsQQPQLbLpzCPQ
SnMSVVZSJMNMZNDVFtJtNRBdqBrWrRHWZTllrrjgHq
ZqdqcrPqqrwnQqnrZqjVcqrQwwmNbzNzwbNLzvFbHLbNmBLF
LCDsCsRTfLTDszzNbsbNNHbs
gLfCgShfCgMPlPrVcqrQgn
QSNSLDQDLfqqPwwBNLqgqJMMmmRRCTzHnCHhRzHmfCmh
lGvdbdWdVvsVszpDhHmmlnMpTC
ctbdtVsbbvvsbWZFdVQJqPtgLQBDBwBQPJwJ
dggSSDCPddRWPnSSPWRDgdSrTDsDQDTzQGGTMbsMMVsQfTfV
jmBvtFpBcBhhjljZHphztMsCbsTTCbzsqGqsfz
hccpLmhFlcwCrPLrCPnL
MMHZnGrCfJnfCPggSSGGSSSgLW
qhFhRlDFDlqFsgdvJdWdDdcSvp
wVlhqTbbRNFqswlVVRNbZfHCrntMBrTTZnJMCfnn
sHGZscVGHJMtmRrqzRqqqTqt
SjvvNgjLShWWhhSQNWqmrBzlRllTTgBqnRmq
LNQWLfWhSQvLdCddWWPHMcbHHrJcDGFZCJssFM
mSDjSVQbVGbmqDVbHmqqJTZzPHTHhhRJhwsRPcRJ
tFfFFttFdsNntfpMMppJWwZTzJczJcZPzwWcdJ
vNtgCrpgNptptgCCbbmjbSbvsVjSsjGG
VCQlZJCTPRWsBsjTTT
wvNrnbbvnhdNhLMfGsrGpRFpGpjp
dwndHbHbbLqwwhNcLsqSHSCHJClQtJSttQDPSJ
ZlrvrdvpGBBhlDrshdqJHRPHqPTJzRPPqw
tcftfSgFFgcgLPmPmpnqFwJRHP
pWLCcWNNNNttMNgZvlsvrBrsrjWDjB
wgdCJgDMDwMCwDMCMJsJJfpffVpVfbfrrrrgjgllZp
QFRhvttRthtQzzmpBWbWzWZSVpbSpl
btRttRLttGNqvbFHLwCdDcMwnPPJDnDD
VhmMNllLqGLJQNhRfZHgSPfgSPTqZj
sBwDcwBtsdzvvHZRlPRjDZTgPZ
pWvvBBcBCdzNLVQVWQlNlW
NsSppvSjSPNBNLJJLh
fCGtqQbZZGZQZTbtzbqbCZThddcMBddlJGhdlBMcddgLlJ
zZFTqwLtTRFqTQwvmprnRVSsDrnvVR
FttFTzzvlVHFzTjpbvzbFSDDdVGhdqLGWGJdVDDfsLqG
cmBNCRnwsCcBPMfLLfJGcWhWqfdh
BwPmZZMmZMCsnrwMrmbHHbjbSvSbjvrlHpzj
sZQHCBFHQQQPGQCCHCHwsHFshhtSnnqjbRSSPngnhbRjqVPn
mzLvmDvNNWvNvrzzrMTzJNjqndqbnSnnRgTdtjdbjhTt
WzzWDlJLzLDvMWJJlMzmLJWcpHFQBpBgBHGQGBHfwwBfQQlG
gdpFrdrmrDsqqswdtccgWWCMlChSbhqSlCzBlSqh
TTvjrfjNJPnRQNTQjvNnCSWBVBVbClPSVSbSVhSh
HRvnfTfvjjHZTDsmcHDsrDsdmp
bFChjhbpbjqsntjtns
vdWcfMHfddvrlNMNdWWTNgBqDngBBZBBQZshgSfgnD
JlwrlrlhlcJWcWMwhWNVFpLzwPbbLRFPppVLzm
DtBtgLvgcHzllsTwzSTg
vhhjZrCrZdVdZVSwPMwwTTMGwT
nmpfqrnZJbqBBvRc
nMvSLvWSWPVPvWnSLShFLBjVbpNVGGbVQbbNcBcBBc
sTzJsJszbbQbdQJb
DsDrwTtsCTFhLQSShRwh
RNFQhTQqHNNGRsNqQFNsHhFCwwPLwPqwzfPrrPBwpJSJJw
vMMMblZjddlvWbjbBBfbwCrPPLJppwCL
jDmvcDBlBdjVglgddmvDQRNFtFRGtQhstHNsGFHV
rhLHmZnMrRsZSstZLLtZnhSCNbbmPJVcblTNNTlccpNTjJTj
WFgGddGFFgFDddMblpJjlTJTPc
QGWqBBfWgBqWwFwzMGvzDqSSrHnCHsrssCRZrfRhHLfH
HHzcWqNPmZcqFHPZGBdMRBMDlllWpRDJMl
tTgSvPhbMDJlbJQb
SCTtvtSPftswjvPhTgffVqmGZLmqmCCcqZzZHFNznF
QNpppRrdZvdgzpQZNpgRRgbSwmDDvFGGqwJSsvSGSqGG
HchWBMcBVnnWcHPjHhWcjHTqJFDGMSSqDMwJJbGwSsGGSb
tcCVcBjPjhnWlFrCFNZflQNr
HsVMrqrPqvvgprSrLG
THJWBJDwRFvBgGSzgF
DmhfHnmQncMNVMqPqbcd
SqZmMJqvHJBhHJLp
wsgTVTSsPssjjFVrTrFlhLhCFlBBnHplHLLHfF
zgggdwPrRrsrjjgRwVwdwdQTmvMvZqDZbWqqMSNWNbGQbGZN
fBDBfLZnTLZVVmmDcQMDDV
jPFtJFpHpJqfJFrptwrJdRWRWNpQVmQRMWNVVVNVvQ
zHwJgtFrTlslfghf
wMwTttCCTTSTfBmPzPVZnPZLVVtbnN
ldRRRlRHggGcvcRbZsNzvBVWnnPBWv
hdlJHgpcJccJhQdJrcrhwFMpDqCwCMBqjSqjqpTC
fJfnwJJnnHJgJHTgjsjDccNjcbgNjm
VdLqRRqGVqpRrPpppMBjDNmDctdsBlNjmZdZ
PDQvPQSvpGDrTwfJzzfFnTnS
MnHvnHHMRMzPTlDLPPRGcl
dFnfhFVwhdBPBfGWlPcP
JNrQFsnVtwsgvNzvmMjpzS
BZVPFpNpcNZpmRRPpzcVNhLLnssDjjDGnqjjLDFDjq
mMJbJvtJQQHlJDGCDnjvChDSsv
MQwWJHdQwWrJltQrgfNPmfBcBrpBpZ
ZWZqDsZZqWsWvWLPwPbpHjdtSbSjSCSPPSCp
MFVNMLmFmNzcTTrFrLbjdjbpCdCSbTCShRSd
czNzLrznlGNNrMzMwDlJwJWDwJwqJDvW
GlgchGGVShlQcQfDhzZrNFnFNFNjFzNFcn
dwCtpwHTtPTWpdFNfJJzRzvJNR
tLBBmWHftBttPbLwCHWTsSSQVglqgMsMBDSMGlQS
RDDDGhGfvPPTTPTThn
ZFLMmjpCpfMZzFqmqsCmPjdVBlVBVnWBPNTVbnTV
zHqJMCzLvftRQQHG
nTcbnvPsvdvFzpczVZmMGg
BCCJwSDqhQLJmMMpzGZVFVFB
qhrwJwrJrrzJNqwWLsTTnlTlbnsvbstWsW
vHRbqPJZvRPZhShJvTZllZtgzwlfBGBlsm
VdQjVVCssQVrWrQmTBgBzglmgCBGml
NnpQNpcFpNWshPRLsbSFsH
cVGmVZVwVVMLdvcRttTdbB
ppCQrwzHBtLrttLb
hsFJQzFWCpCqjZGVwlhlPP
HDGRzgWhgfzVWfRpspwRwbwStSwt
ZBPPPmmmTMQMPcZrBmSptSbbQCwtlsNqCwjC
TTLMMmZvPTrMZvFMmcmvrTccDDnfGHJgJhHhnhnLfVhSWDJz
pNrpjzthZPnGrzzWbJLLLbbJZwgSvZCV
MQsFFFDTfMNfRFfBFMdBdwLSvgbSTVCqTgVbbTLvwV
BQlQDMFccQsNmWpPGhpcjr
CTgGRCRglLlLTllL
vMJmhPJcmPBMvhqPDnNNqlWnwDWqsRQs
hcBfcJRPFfvvRvJZBrfMPZdpbSSGtSdtdgtzzSZzbV
NQLzNzzJcrLrSgZSSGgZrR
bTsjqHvcmTHvjgZGDvDpGZRfpg
WqTVPbdnMlLJncQC
hZLBrqLGLMbzLLBhfMMrnnNJlnNnlnJJNNdCJdzN
TWTsWqvtvpTSgRHpVFdjgjCPdgJlCFCFnF
swSTsTwpTVRmVRRRqMDMfqfDwLfbrhLr
NTQHWNQWrQwSTDWlcPPBHZBZbPgZJZ
nmfjCRCfRhndJcjBbcbcbg
nsppRfssfzCnqgzTzrwTwQVTWM
mFjQmDGmbbGjmChrCwdQBHCHWh
qvZZnPvvnngMpnlqpMZnpsTgWHTRCWrVdVDBWRhHBrhHDHhd
vqZgnnqgLvqlPllpnjGDjmNjNLfftftLFD
rfGsjsMNnFMMFddMsttDMgLHGlmJLCPPmHHGmHPlmm
vZcbhQbrRbVZPJLwPTgCLlgb
hchzSBqzQvphnWnrjFdWMqff
WmfPWfVsfqszRDqPqgpvHhvdwddGMmGghM
QtTrtTcSBjtQCctStrTrzhpwjGvGHhngwMHGHvMv
TtTQlQFcSSJlcccBbltQQTTRsPZDsWzRFzWFzPNfsssLPs
QpNNMrjcNMccGNdvLBBlBsBjnsnF
tTSqbbbqCtWWCTWSVTmmCJPwVwnwvFPnsPVnnPfddlvf
HmhJTZWqHqCJJJltqpNGRgzZDcQNrgzDGM
HcLVRhhTRsLRRVjslTscqNQmVNQQgQttqNwNZtmw
nJdBJJhfFPSCbJBJBMbzFbFgmNmtgmvgNgnntNwZQQNNmw
bMbPzJbzCBPrJfdfbBbdCrGHlLTTpWjsGhGTTRTRlc
sJCCpQJQCrfCfnSCrT
vmqgNggzgmZqmPShqBhThfhDhjDhhB
RZNzHRzZSQwHwHVVWc
jtVtvVHgvjJbHjjQPMZdCcwlMdNbdFlNlc
WppSBDzGfBzTBqQWwCFMlwZMwMcZ
zBfnqpRGnSSqTfqpTpSnnHQsjJgQvPJshHtHVh
qJMRMcPPVzVhmsDWfhWT
BglQBNlgZtQBHLHHBnTjWSWmFmwDmWjSsnmF
BdHvgHBvBtZbTpJRPCdcdpGrGJ
pcGcWGWlvQZpzmDbgFmz
HqqnddDdddjzTTggjZgFtT
sHqRwrRsJswLHrMLLRJdqNVVrGffPGWcvSSWlDfGfc
lttTbgRvqvtQRhjLzGjLVh
JJfrHfrdffZJQmZhLLZVVwFj
sBjCfSNNTTqnCnqD
qMtWjSrHftGfjqrJGMqzVzFmBBrzQQwzgBVQVQ
LDChPbThbTcTpCTcnPPQPQzVPvvzQBBWgVBQ
ZLspppLpdZZttdHttqdWMf
htJcJhpMQQWjhNWdJQSCFCTvFBPCTDlMmDCFlM
jjbbsfjwZbLGVVqHCFPvmvDmClTfmP
zjVVRwZwnRJtnNQt
PCPVSzLMMRqGwgMmHmQmDQ
slrrbZZgsfcdsgdhrHFGQHQFwvfwFwDGTv
NclhgpctrrNjllcZdcrpZnPPqzLLSSLqJLtJWCWzCn
PBLSBPVBwpTVppfT
lZCqQQtCQGPJJPtPHHwTwZTTZpwHsfRH
mCtGFDqFGDGQjPGqjJMMlqPgdWgSSgBWWcWzLdgvMzgBMg
cLBrfchhFBcnrgvqvPGvvwSS
QpzpstDDZMwDZqwh
WzpbWTjsbhpQtjThsjJFRNLnfLbfRLRBLlFB
ngnWWqnfgqtfsrWftqsrFWPSdSSdRCTHRSwpRGTfGmSG
VhJhVczJQcvbvvlhBpvlPdmlwHRTGHSRPTSCRGTd
zVcBcMhzcVcvMJJJDpWrsqrtrWLWsgZZFtFD
fbccrJlrffTwJDJTtBtB
hRNNFddsgsFPLLRVVwthMCQTtBwrtT
jrsPGLNjsqPlvGbZbcvScz
HFPmmgQrQzFgrLVPPrLFPNDJNJzzcGbJTbsSzbGGNc
MtvCMhJBdnMhwfhlwnfBfMDCDSjGbqDNGNGqGjDDjsDC
wwnhhdtBBptwdlhlRntRldJFVWRFPmWZFmHRZZmFQPRWmm
WrHNNTBNTTTBwHHcSTrBnSzJPFnpJfpLVfpDVdJLFJLFdD
hRthQvhRQlQmDpfVJFdLlLLj
hMZZbCMvgQgBTBGNGDcWbr
HvQjMRMTzjsCQzHTCFfVVZLPVvfLfPVpZg
GtlbBtSGlSbDdStrhSFCPVDgZgLLgPpPJWPF
rmwSbcbcdbrbGljQjCzCCwnHRqQT
bbgNSHPPgnmMMZtNcMpp
VFzFDFVtCBFDCVFdMlhZMhdhmhmplwZL
JVtBjGRFRttFCGDFGJJDQQgSgTWPPfSSfWbQnHvPWW
NvdBpwNvGNFvpBGGBmLFblrtVTwDttlhtlblfQbQ
SCMMsWCMSRZCqsmWcRWgRRsVtlrtrQbtQftThrQTQtqrtQ
WRscMgZJJCJWzZgSWNLFdBNHGzpFGmBFFG
qghqRVzhLNRLqzLhVztgQdLFdrccCnSpcZdSZcTS
DwvmHDJDsmvDGmHbQBlslMDDCrTCnppTndrdBrFnFTSdCCnZ
GGwJHlGwwvMHJljwwDMVtfhtWWhzqVPPjfQRqz
BsDMPrqPzsDwwCLGmqjpjm
VfFJQlVQcvfwJLJCJppLNp
vfcSHCglCgbgbbbFvSlvQfPsrsZrPzZzDWWStPhtZPDP
gjMsnFgbnllbjMfSZBHHtpHvvvFwhv
DDRZDLdVCLNLJwBCShQHHwwBzv
DNJNTLJRTqWmjWZnjrlmjW
ZTSVSFZCLTnvzfzqvnNL
PfPcJljfMpvtlnztvQtw
PsJMMMWpGcgMHMfjRBThrgrTbBSBFdVSTF
GccBRWjgtQqsTcVQcw
JhJCMJHPLffMChlfLCLHMMrDQsQqDVQsqTDbVvGqDhhzqD
dHGlfnCHlJrNmtdptggpmW
wnDDSBCSBSDLzLLmHLrlwlmpTTqzGJJfpjfjNpfqbpbdpG
MMRhFWWRvZPZRZQhFZMVhVSNqjqpNQffJjbjfbjdTJbp
VMMsWcZRWgMPvRSrSHmsrrtwSHnr
TQchPTgjBcNgPHhhThtNzQdzdsCmRDJnzCCmCdCm
vllVwrfvbSBVFSbVGwlrFGlqRCDzRJCJdzvJRzsdLDDLsdCm
VrMrqSWbfqWbBhhpWjNTttpjjP
rsfvSHHcvwrMPtcQZgnDhGdvJzngLzzJLJ
lWmVlfbCCNFCpBCmTpFFJgzhDLGhmRGhLhmGdgGR
pNBfflVTNpfWTWbWWbjNVqBsscqsqrZSwMwZrMPZSZrZrs
PJPHPJmhhHhlHPQgCndngTbWnqCWDGTD
tSwccFpFqwMcFbGFWvnnWvCW
MwLwLMSwpNBBtctSctfhZHJQhhqmlRlZRNPH
GNzdZhVGvtGZVVgGgtfHHWhpLPPpLWpWWnHf
RjwqRcDTvCrWJWWnlLnnqn
DbrDDwwBwjjsrbDTRTBmwgZmgGgmdttvQvQFSQGFtg
jRgcZRfhmHfZjPZRgHffLFTzzddBTBBFzLDZzBTF
VtsJwSbcStlwMqbtwbvWBWddGGdrFDDWJWrzTT
VwsQQvlbVbVlbNllVwbMmmpnjpfChfQpnhfcCCnH
dFnFjWjTQTFzFWPWPgqhRQRqgVhRqfRqQJ
bStrbpmNGHSrBDmrNBtHBhMVLLqLqVVglrllPVLgPg
tSsbBDmBbbGmmSHDbtmHbtNCjnzscZccjnPcTcdzWcvjswFz
lFCjDhqggMlDvMhFDgqFFzHHwHwwwTpLBwmwqmmpBpwT
GPdPnStGncQGNStZPpBmVZmRmfzTRmVVfL
tWtNdPWzsbtMDCbrCbrjrv
BJHMgLlcMTBLCtbqmMDGppmmMM
ZFPsrrdvwrNvrdNZsvhrrzzRSmJRbJSmbztsmpRSSm
NwhfPZFNdFQPVQdvZFNgjglJLTCQngQWllBcTT
jGlQQvQvpRQRGfnPLfcfGTnP
BMqmdBVBwmFdVMFZdcTPqgLnnggTTLSzPS
FVtMMVcbZVrcZMQCHjHWJJCJDvrW
rPPwVwbpRbbVlllTLCTRqTLL
dNdZssBBCBszHsjhDTQgqLDvlTgDZgll
dSsCNNHMdsdWWWmpGfmPFS
rzCLrsjgZjwcwSZc
wNBNRJpRltHNWWRHBlGlJtRcTZSVBmZDVqZTfBVVTDTVTD
NWPtGJPNGWHvpvtwvWgzQvdvQQzhnsnCvCLM
HHbJhzddMPbPgnDWbZ
BLnjLNvBrrcvvvwnwLrnqrgpPRgRNCWgZDPPpDgRpRWp
jtsBqScStfJQnnVF
QVFSVgQFZZQlQqQSlgQpRppSbRTSTppJJbRpLb
cGwCDwjrnrGvzBzGnwwvDBjnpLbsLTTqRPbsJPMJMWpPns
tcGzrCdtGdQmVZqVNQ
RtTRhncVMTVccShRTctLdfPdJpLPqJhZphHpJs
BzssCmFNWWqWwqwPLH
svzvvsmmFBmsggrGlGMVSMtMRRncSQScRRRl
rmmqrQQwLbbGrrGr
cNJzzzWtWmLCGGbLWWbv
cVtMppchzMBVMcNJcMsRwqZFMlgmggmRgg
mQsQBHFMrbddbRqH
NzhcQNfNNtzvWwZdSrgbrprPrwLbgb
zcJVhTtNNcvcfVZmBmQMGMMljTCmlB
FlldqjSlCgfvPFfvFF
rbnDtVBMbprTsbVVcTDTrpMcmNwgHPgghTmNfLwvfPNLwhdT
drMppdnbbtQDBtbnsBbcrrtbSqSSRCjlQZWllllSRlWRGCCC
nqdCsqbbwdsrHFVJHcwFTc
jPPjtWjPWgRltRLsBRrNpHFDHVFWVVJNNHrD
fgllPGQjBffLjtzSsqvbSSzGvnhS
zsVBzMfHHnzlwwVlqcJJFT
ZzRLvLDzQzTmlWlqWRWF
GbQQvpGvSSpjdQjSQZpQZGLfrgBCsHzrdtCsnfCBHsBgdH
zBLbLWzqqwLMnMZTnHlnsHTvFlFHNT
fjhdcrjjdVdrGSmmdfccGclPvlvPTlGHTFgNvNgqFFvg
pmmcrcRrjSVJchqVccjpRwZMDwCJQBbLDCCbwBWLzL
TDMBgBgLlcjBfMfcVJVmGnnJjvPVCPVv
zzptqHstJqFzzdJJZNvNpvNpnNvGnNZm
dHszrWQhdzHQqdztwQBLSfglfDbfJlJTLg
VTmvrldtGGwmlvmGDHlLnFDCCplFQHLH
ssgjzSzzJCQSSFVVQF
WsRWhgVqRtfvwcddhc
bdlDwznhnNlffMcPTPfzzQ
srCRGRrZCmVTBfBBfTQcZb
brSrrGvRVvWmRsrHrWSbjNJwdDFhnNlwtlnSdnhN
QQqqRfdQQSdjgPmZfBmmPgRhphphJtLmJhTJJhVbTtLhTb
vvlNGzDDDcslcsGDlWHtCFVpcCbThFTtbJFtCh
DrMGlzMVwNGWsWMHDMvlzlMfZdQdQPZfSZRfdrPBfqRZgj
qVHfHNJCHVvvFFbfFlHHnCQQDhLnhhhPZrZnPZPn
mSMszWRMQmhqrnZL
GjtzjSSdRGSjsRtdRMttgGgsqqFNfFcGVvVVvlbHFFGFVFwb";

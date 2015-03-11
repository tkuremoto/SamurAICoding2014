#include <stdio.h>
#include <vector>
#include <algorithm>

using namespace std;

#define INTMAX 0x00ffffff
#define NTURN 9
#define NDAIMYO 4
#define NLORD 6
#define NSTRTYPE 4
#define STRTYPE_TOP 0
#define STRTYPE_TOPTIE 1
#define STRTYPE_MIDDLE 2
#define STRTYPE_LOWESTTIE 3

#define NDAY 5
#define NNIGHT 2

struct lord
{
public:
	int id, strength, rintimacy, intimacy[NDAIMYO];

	bool operator <(const lord& r) const{
		if (this->strength == r.strength){
			return this->id > r.id;
		}
		return this->strength > r.strength;
	}
};

struct strategy
{
public:
	double score;
	int cost;
	strategy(){}
	strategy(double s, int c){
		score = s;
		cost = c;
	}
};

struct GInput
{
	int strength[NLORD];
};

struct Input
{
	int d, intimacy[NLORD][NDAIMYO], rintimacy[NLORD], negotiate[NLORD];
	char p;
};

strategy stable[NLORD][NSTRTYPE];

int deflimit[] = { INTMAX, INTMAX, INTMAX, INTMAX, INTMAX, INTMAX };
double memo[NDAY + 1][NLORD];
int memo_str[NDAY + 1][NLORD];
double dp(int cost, int n,int limit[NLORD])
{
	if (n < 0){
		return 0.0;
	}
	if (memo[cost][n] > -0.1){
		return memo[cost][n];
	}
	if (0 == cost){
		memo_str[cost][n] = -1;
		return memo[cost][n] = 0.0;
	}

	double ret = dp(cost, n - 1, limit); memo_str[cost][n] = -1;
	for (int i = 0; i < NSTRTYPE; i++){
		if (cost >= stable[n][i].cost){
			double tmp = dp(cost - stable[n][i].cost, n - 1, limit) + stable[n][i].score;
			if (ret < tmp){
				ret = tmp; memo_str[cost][n] = i;
			}
		}
	}

	return memo[cost][n] = ret;
}

const char* thinknoestm(GInput gin, Input in, char *buf)
{
	//Initialize.
	lord lds[NLORD];
	int revidx[NLORD];

	for (int i = 0; i < NLORD; i++){
		lds[i].id = i; lds[i].strength = gin.strength[i];
		for (int j = 0; j < NDAIMYO; j++){
			lds[i].intimacy[j] = in.intimacy[i][j];
		}
		lds[i].rintimacy = in.rintimacy[i];
	}

	sort(lds, lds + NLORD);

	for (int i = 0; i < NLORD; i++){
		revidx[lds[i].id] = i;
	}

	for (int i = 0; i < NLORD; i++){
		fill(stable[i], stable[i] + NSTRTYPE, strategy(0.0, INTMAX));
	}

	//Make strategy candidates.
	for (int i = 0; i < NLORD; i++){
		int amax = *max_element(lds[i].intimacy + 1, lds[i].intimacy + NDAIMYO);
		int amin = *min_element(lds[i].intimacy + 1, lds[i].intimacy + NDAIMYO);
		int nmax = count(lds[i].intimacy, lds[i].intimacy + NDAIMYO, amax);
		int nmin = count(lds[i].intimacy, lds[i].intimacy + NDAIMYO, amin);
		if (amax < lds[i].rintimacy){ //Apparently, I'm the highest.
			//Do nothing.
		}
		else if (amax == lds[i].rintimacy){ //Apparently, I'm the highest, but tie.
			stable[lds[i].id][STRTYPE_TOP] = strategy((double)lds[i].strength * (double)(nmax - 1) / (double)nmax, 1);
		}
		else if (amin == lds[i].rintimacy){ //Apparently, I'm the lowest, but tie.
			stable[lds[i].id][STRTYPE_TOP] = strategy((double)lds[i].strength + (double)lds[i].strength / (double)nmin, amax - lds[i].rintimacy + 1);
			stable[lds[i].id][STRTYPE_TOPTIE] = strategy((double)lds[i].strength / (double)nmin + (double)lds[i].strength / (double)(nmax + 1), amax - lds[i].rintimacy);
			stable[lds[i].id][STRTYPE_MIDDLE] = strategy((double)lds[i].strength / (double)nmin, 1);
		}
		else if (amin > lds[i].rintimacy){ //Absolutely, I'm the lowest.
			stable[lds[i].id][STRTYPE_TOP] = strategy((double)lds[i].strength * 2.0, amax - lds[i].rintimacy + 1);
			stable[lds[i].id][STRTYPE_TOPTIE] = strategy((double)lds[i].strength + (double)lds[i].strength / (double)(nmax + 1), amax - lds[i].rintimacy);
			stable[lds[i].id][STRTYPE_MIDDLE] = strategy((double)lds[i].strength, amin - lds[i].rintimacy + 1);
			stable[lds[i].id][STRTYPE_LOWESTTIE] = strategy((double)lds[i].strength * (double)nmin / (double)(nmin + 1), amin - lds[i].rintimacy);
		}
		else{ //i.e. I'm 2nd or 3rd.
			stable[lds[i].id][STRTYPE_TOP] = strategy((double)lds[i].strength, amax - lds[i].rintimacy + 1);
			stable[lds[i].id][STRTYPE_TOPTIE] = strategy((double)lds[i].strength / (double)(nmax + 1), amax - lds[i].rintimacy);
		}
	}

	//Dynamic plannning to maximize the score within the cost.
	for (int i = 0; i < NDAY + 1; i++){
		fill(memo[i], memo[i] + NLORD, -1.0);
		fill(memo_str[i], memo_str[i] + NLORD, -1.0);
	}

	double maxscore = dp(NDAY, NLORD - 1, deflimit);
	int rest = NDAY, cnt = 0, ret[NDAY];
	for (int i = NLORD - 1; i >= 0; i--){
		if (-1 != memo_str[rest][i]){
			int tmp = stable[i][memo_str[rest][i]].cost;
			for (int j = 0; j < stable[i][memo_str[rest][i]].cost; j++){
				ret[cnt++] = i;
			}
			rest -= tmp;
		}
	}

	//Can such a situation happen???
	for (; rest > 0; rest--){
		ret[cnt++] = lds[1].id;
	}

	sprintf(buf, "%d %d %d %d %d\n", ret[0], ret[1], ret[2], ret[3], ret[4]);

	return buf;
}

const char* thinknoestmnight(GInput gin, Input in, char *buf,int limit[NLORD])
{
	//Initialize.
	lord lds[NLORD];
	int revidx[NLORD];

	for (int i = 0; i < NLORD; i++){
		lds[i].id = i; lds[i].strength = gin.strength[i];
		for (int j = 0; j < NDAIMYO; j++){
			lds[i].intimacy[j] = in.intimacy[i][j];
		}
		lds[i].rintimacy = in.rintimacy[i];
	}

	sort(lds, lds + NLORD);

	for (int i = 0; i < NLORD; i++){
		revidx[lds[i].id] = i;
	}

	for (int i = 0; i < NLORD; i++){
		fill(stable[i], stable[i] + NSTRTYPE, strategy(0.0, INTMAX));
	}

	//Make strategy candidates.
	for (int i = 0; i < NLORD; i++){
		int amax = *max_element(lds[i].intimacy + 1, lds[i].intimacy + NDAIMYO);
		int amin = *min_element(lds[i].intimacy + 1, lds[i].intimacy + NDAIMYO);
		int nmax = count(lds[i].intimacy, lds[i].intimacy + NDAIMYO, amax);
		int nmin = count(lds[i].intimacy, lds[i].intimacy + NDAIMYO, amin);
		if (amax < lds[i].rintimacy){ //Apparently, I'm the highest.
			//Do nothing.
		}
		else if (amax == lds[i].rintimacy){ //Apparently, I'm the highest, but tie.
			stable[lds[i].id][STRTYPE_TOP] = strategy((double)lds[i].strength * (double)(nmax - 1) / (double)nmax, 1);
		}
		else if (amin == lds[i].rintimacy){ //Apparently, I'm the lowest, but tie.
			stable[lds[i].id][STRTYPE_TOP] = strategy((double)lds[i].strength + (double)lds[i].strength / (double)nmin, (amax - lds[i].rintimacy) / 2 + 1);
			if (0 == (amax - lds[i].rintimacy) % 2){
				stable[lds[i].id][STRTYPE_TOPTIE] = strategy((double)lds[i].strength / (double)nmin + (double)lds[i].strength / (double)(nmax + 1), (amax - lds[i].rintimacy) / 2);
			}
			stable[lds[i].id][STRTYPE_MIDDLE] = strategy((double)lds[i].strength / (double)nmin, 1);
		}
		else if (amin > lds[i].rintimacy){ //Absolutely, I'm the lowest.
			stable[lds[i].id][STRTYPE_TOP] = strategy((double)lds[i].strength * 2.0, (amax - lds[i].rintimacy) / 2 + 1);
			if (0 == (amax - lds[i].rintimacy) % 2){
				stable[lds[i].id][STRTYPE_TOPTIE] = strategy((double)lds[i].strength + (double)lds[i].strength / (double)(nmax + 1), (amax - lds[i].rintimacy) / 2);
			}
			stable[lds[i].id][STRTYPE_MIDDLE] = strategy((double)lds[i].strength, (amin - lds[i].rintimacy) / 2 + 1);
			if (0 == (amin - lds[i].rintimacy) % 2){
				stable[lds[i].id][STRTYPE_LOWESTTIE] = strategy((double)lds[i].strength * (double)nmin / (double)(nmin + 1), (amin - lds[i].rintimacy) / 2);
			}
		}
		else{ //i.e. I'm 2nd or 3rd.
			stable[lds[i].id][STRTYPE_TOP] = strategy((double)lds[i].strength, (amax - lds[i].rintimacy) / 2 + 1);
			if (0 == (amax - lds[i].rintimacy) % 2){
				stable[lds[i].id][STRTYPE_TOPTIE] = strategy((double)lds[i].strength / (double)(nmax + 1), (amax - lds[i].rintimacy) / 2);
			}
		}
	}

	//Dynamic plannning to maximize the score within the cost.
	for (int i = 0; i < NNIGHT + 1; i++){
		fill(memo[i], memo[i] + NLORD, -1.0);
		fill(memo_str[i], memo_str[i] + NLORD, -1.0);
	}

	double maxscore = dp(NNIGHT, NLORD - 1, limit);
	int rest = NNIGHT, cnt = 0, ret[NNIGHT];
	for (int i = NLORD - 1; i >= 0; i--){
		if (-1 != memo_str[rest][i]){
			int tmp = stable[i][memo_str[rest][i]].cost;
			for (int j = 0; j < stable[i][memo_str[rest][i]].cost; j++){
				ret[cnt++] = i;
			}
			rest -= tmp;
		}
	}

	//Can such a situation happen???
	for (; rest > 0; rest--){
		ret[cnt++] = lds[1].id;
	}

	sprintf(buf, "%d %d\n", ret[0], ret[1]);

	return buf;
}

const char* thinkturn0(GInput gin, Input in, char *ret)
{
	lord lds[NLORD];

	for (int i = 0; i < NLORD; i++){
		lds[i].id = i; lds[i].strength = gin.strength[i];
	}

	sort(lds, lds + NLORD);

	//In the 1st turn,
	//Meet the most strong lord.
	sprintf(ret, "%d %d %d %d %d\n", lds[0].id, lds[0].id, lds[0].id, lds[0].id, lds[0].id);

	return ret;
}

const char* thinkturn1(GInput gin, Input in, char *ret)
{
	lord lds[NLORD];

	for (int i = 0; i < NLORD; i++){
		lds[i].id = i; lds[i].strength = gin.strength[i];
	}

	sort(lds, lds + NLORD);

	sprintf(ret, "%d %d\n", lds[1].id, lds[1].id);

	return ret;
}

const char* thinkturn3(GInput gin, Input in, char *ret)
{
	lord lds[NLORD];

	for (int i = 0; i < NLORD; i++){
		lds[i].id = i; lds[i].strength = gin.strength[i];
	}

	sort(lds, lds + NLORD);

	sprintf(ret, "%d %d\n", lds[2].id, lds[2].id);

	return ret;
}

double calcscore(const GInput &gin, const Input &in)
{
	double ret = 0.0;
	for (int i = 0; i < NLORD; i++){
		int amax = *max_element(in.intimacy[i], in.intimacy[i] + NDAIMYO);
		int amin = *min_element(in.intimacy[i], in.intimacy[i] + NDAIMYO);
		int nmax = count(in.intimacy[i], in.intimacy[i] + NDAIMYO, amax);
		int nmin = count(in.intimacy[i], in.intimacy[i] + NDAIMYO, amin);
		if (amax <= in.intimacy[i][0]){ //I'm the highest, or tie
			ret += (double)gin.strength[i] / (double)nmax;
		}
		else if (amin >= in.intimacy[i][0]){ //I'm the lowest, or tie.
			ret -= (double)gin.strength[i] / (double)nmin;
		}
	}

	return ret;
}

double nighteval(const GInput &gin, const Input &in, const int negotiation[NLORD], int ret[NDAIMYO][NLORD])
{
	int table[NDAIMYO][7] = { { 0, 0, 1, 1, 1, 1, 1 },
							{ 0, 0, 1, 1, 1, 1, 1 },
							{ 0, 0, 1, 1, 1, 1, 1 },
							{ 0, 0, 1, 1, 1, 1, 1 } };

	double e = -1.0e100;
	int tmpret[NDAIMYO][NLORD];

	//21^3 = 9261-loop.
	bool end;
	do{
		for (int i = 1; i < NDAIMYO; i++){
			int cur = 0, cnt = 0;
			for (int j = 0; j < 7; j++){
				if (1 == table[i][j]){
					tmpret[i][cur++] = cnt;
					cnt = 0;
				}
				else{
					cnt++;
				}
			}
			tmpret[i][cur] = cnt;
		}

		bool flag = true;
		for (int i = 0; i < NLORD; i++){
			int neg = 0;
			for (int j = 1; j < NDAIMYO; j++){
				neg += tmpret[j][i];
			}
			if (neg != negotiation[i]){
				flag = false;
				break;
			}
		}

		if (flag){
			double tmp = 0.0;
			for (int i = 1; i < NDAIMYO; i++){
				Input iin = in;
				for (int j = 0; j < NLORD; j++){
					swap(iin.intimacy[j][0], iin.intimacy[j][i]);
					iin.intimacy[j][0] += tmpret[i][j] * 2;
				}
				tmp += calcscore(gin, iin);
			}
			if (tmp > e){
				e = tmp;
				for (int j = 1; j < NDAIMYO; j++){
					copy(tmpret[j], tmpret[j] + NLORD, ret[j]);
				}
			}
		}

		end = true;
		for (int i = 1; i < NDAIMYO; i++){
			if (next_permutation(table[i], table[i] + 7)){
				end = false;
				break;
			}
			else{
				sort(table[i], table[i] + 7);
			}
		}

	} while (!end);

	return e;
}

int main()
{
	//Get ready
	printf("READY\n"); fflush(stdout);

	//Read game settings
	GInput gin;

	scanf("%*d%*d%*d");

	for (int i = 0; i < NLORD; i++){
		scanf("%d", gin.strength + i);
	}

	int cur, negsum[NLORD], mynegsum[NLORD], nightestm[NTURN][NDAIMYO][NLORD];
	for (cur = 0; cur < NTURN; cur++){
		if (5 == cur){
			fill(negsum, negsum + NLORD, 0);
			fill(mynegsum, mynegsum + NLORD, 0);
		}

		//Read input
		Input in;

		scanf("%d %c", &in.d, &in.p);

		for (int i = 0; i < NLORD; i++){
			for (int j = 0; j < NDAIMYO; j++){
				scanf("%d", in.intimacy[i] + j);
			}
		}

		for (int i = 0; i < NLORD; i++){
			scanf("%d", in.rintimacy + i);
		}

		if ('D' == in.p){
			for (int i = 0; i < NLORD; i++){
				scanf("%d", in.negotiate + i);
			}
		}

		//Include night estimation of former turn.
		for (int t = 2; t < NTURN; t += 2){
			if (cur <= 4 && t < cur){
				for (int i = 1; i < NDAIMYO; i++){
					for (int j = 0; j < NLORD; j++){
						in.intimacy[j][i] += nightestm[t][i][j] * 2;
					}
				}
			}
			else if (cur >= 5 && 5 <= t && t < cur){
				for (int i = 1; i < NDAIMYO; i++){
					for (int j = 0; j < NLORD; j++){
						in.intimacy[j][i] += nightestm[t][i][j] * 2;
					}
				}
			}
		}

		//Write output
		if ('D' == in.p){
			char tmp[1024];
			if (0 == cur){
				//At 1st turn, vote all for the strongest lord.
				printf("%s", thinkturn0(gin, in, tmp)); fflush(stdout);
				fill(negsum, negsum + NLORD, 0);
				fill(mynegsum, mynegsum + NLORD, 0);
			}
			else{
				int estm[NDAIMYO][NDAY];
				for (int i = 0; i < NLORD; i++){
					negsum[i] += in.negotiate[i];
				}

				int rneg[NLORD];
				copy(negsum, negsum + NLORD, rneg);
				for (int i = 0; i < NLORD; i++){
					rneg[i] -= mynegsum[i];
				}

				//Estimate negotiation at night, given negotiation infomation.
				nighteval(gin, in, rneg, nightestm[cur]);
				for (int i = 1; i < NDAIMYO; i++){
					for (int j = 0; j < NLORD; j++){
						in.intimacy[j][i] += nightestm[cur][i][j] * 2;
					}
				}

				//Estimate negotiation at this turn.
				for (int i = 1; i < NDAIMYO; i++){
					Input iin = in;
					char tmp[1024];
					for (int j = 0; j < NLORD; j++){
						iin.rintimacy[j] = in.intimacy[j][i];
						iin.intimacy[j][0] = in.intimacy[j][i];
						iin.intimacy[j][i] = in.rintimacy[j];
					}

					sscanf(thinknoestm(gin, iin, tmp), "%d%d%d%d%d", estm[i], estm[i] + 1, estm[i] + 2, estm[i] + 3, estm[i] + 4);
				}

				for (int i = 1; i < NDAIMYO; i++){
					for (int j = 0; j < NDAY; j++){
						in.intimacy[estm[i][j]][i] += 1;
					}
				}

				char tmp[1024];
				printf("%s", thinknoestm(gin, in, tmp)); fflush(stdout);
			}
		}
		else if ('N' == in.p){
			char tmp[1024];
			int estm[NDAIMYO][NNIGHT];

/*			if (1 == cur){
				printf("%s", thinkturn1(gin, in, tmp)); fflush(stdout);
				sscanf(tmp, "%d%d", estm[0], estm[0] + 1);
				mynegsum[estm[0][0]] += 1; mynegsum[estm[0][1]] += 1;
			}
			else if (3 == cur){
				printf("%s", thinkturn3(gin, in, tmp)); fflush(stdout);
				sscanf(tmp, "%d%d", estm[0], estm[0] + 1);
				mynegsum[estm[0][0]] += 1; mynegsum[estm[0][1]] += 1;
			}
			else*/{
				//Estimate at this turn.
				for (int i = 1; i < NDAIMYO; i++){
					Input iin = in;
					char tmp[1024];
					for (int j = 0; j < NLORD; j++){
						iin.rintimacy[j] = in.intimacy[j][i];
						iin.intimacy[j][0] = in.intimacy[j][i];
						iin.intimacy[j][i] = in.rintimacy[j];
					}

					sscanf(thinknoestmnight(gin, iin, tmp, deflimit), "%d%d", estm[i], estm[i] + 1);
				}

				////Invest twice to the same lord.
				//double score = -1.0e100;
				//int ret;
				//for (int i = 0; i < NLORD; i++){
				//	Input iin = in;
				//	for (int j = 0; j < NLORD; j++){
				//		iin.intimacy[j][0] = in.rintimacy[j];
				//	}
				//	iin.intimacy[i][0] += 4;

				//	double tmpscore = calcscore(gin, iin);
				//	if (tmpscore > score){
				//		score = tmpscore;
				//		ret = i;
				//	}
				//}
				//char tmp[1024];
				//printf("%d %d\n", ret, ret); fflush(stdout);
				//mynegsum[ret] += 2;

				for (int i = 1; i < NDAIMYO; i++){
					for (int j = 0; j < NNIGHT; j++){
						in.intimacy[estm[i][j]][i] += 2;
					}
				}

				printf("%s", thinknoestmnight(gin, in, tmp, deflimit)); fflush(stdout);
				sscanf(tmp , "%d%d", estm[0], estm[0] + 1);
				mynegsum[estm[0][0]] += 1; mynegsum[estm[0][1]] += 1;
			}
		}
	}

	return 0;
}
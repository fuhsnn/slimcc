#include "slimcc.h"

int32_t eval_bitint_first_set(int32_t bits, void *lp) {
  int32_t idx = (bits - 1) / 64;
  uint64_t *lh = lp;

  int clr_shft = 63 - (bits - 1) % 64;
  lh[idx] = (lh[idx] << clr_shft) >> clr_shft;

  for (; idx >= 0; idx--)
    if (lh[idx])
      for (int shft = 0; shft < 64; shft++)
        if (0 > (int64_t)(lh[idx] << shft))
          return (64 - shft) + idx * 64;
  return 0;
}

bool eval_bitint_overflow(int32_t bits, void *sp, int32_t chk_bits, bool is_unsigned) {
  int64_t *val = sp;
  int32_t idx = (chk_bits - 1) / 64;
  int shft = (chk_bits - 1) % 64;

  int64_t chk = val[idx++] >> shft;
  if (chk != 0)
    if (is_unsigned || chk != -1)
      return true;

  int32_t top = (bits - 1) / 64;
  while (idx <= top)
    if (val[idx++] != chk)
      return true;

  return false;
}

bool eval_bitint_to_bool(int32_t bits, void *lp) {
  int32_t idx = (bits - 1) / 64;
  uint64_t *lh = lp;

  int clr_shft = 63 - (bits - 1) % 64;
  lh[idx] = (lh[idx] << clr_shft) >> clr_shft;

  while (idx >= 0)
    if (lh[idx--])
      return true;
  return false;
}

int eval_bitint_cmp(int32_t bits, void *lp, void *rp, bool is_unsigned) {
  int32_t idx = (bits - 1) / 64;
  uint64_t *lh = lp, *rh = rp;

  int clr_shft = 63 - (bits - 1) % 64;
  uint64_t signbit = is_unsigned ? 0 : 1ULL << 63;
  uint64_t lv = (lh[idx] << clr_shft) + signbit;
  uint64_t rv = (rh[idx] << clr_shft) + signbit;

  for (;;) {
    if (lv > rv)
      return 2;
    else if (lv < rv)
      return 1;

    if (--idx < 0)
      break;
    lv = lh[idx];
    rv = rh[idx];
  }
  return 0;
}

void eval_bitint_sign_ext(int32_t bits, void *lp, int32_t bits2, bool is_unsigned) {
  int32_t idx = (bits - 1) / 64;
  uint64_t *lh = lp;

  uint64_t fill = lh[idx];
  int shft = bits % 64;
  if (shft) {
    fill <<= 64 - shft;
    if (is_unsigned)
      lh[idx] = (uint64_t)fill >> (64 - shft);
    else
      lh[idx] = (int64_t)fill >> (64 - shft);
  }
  fill = is_unsigned ? 0 : (int64_t)fill >> 63;

  int32_t top = (bits2 - 1) / 64;
  while (++idx <= top)
    lh[idx] = fill;
}

void eval_bitint_neg(int32_t bits, void *lp) {
  int32_t cnt = (bits + 63) / 64;
  uint64_t *lh = lp;

  uint64_t borrow = 0;
  for (int32_t i = 0; i < cnt; i++) {
    uint64_t tmp;
    borrow = (tmp = lh[i] + borrow) < borrow;
    borrow |= (lh[i] = 0 - tmp) > 0;
  }
}

void eval_bitint_bitnot(int32_t bits, void *lp) {
  int32_t cnt = (bits + 63) / 64;
  uint64_t *lh = lp;

  for (int32_t i = 0; i < cnt; i++)
    lh[i] = ~lh[i];
}

void eval_bitint_bitand(int32_t bits, void *lp, void *rp) {
  int32_t cnt = (bits + 63) / 64;
  uint64_t *lh = lp, *rh = rp;

  for (int32_t i = 0; i < cnt; i++)
    rh[i] &= lh[i];
}

void eval_bitint_bitor(int32_t bits, void *lp, void *rp) {
  int32_t cnt = (bits + 63) / 64;
  uint64_t *lh = lp, *rh = rp;

  for (int32_t i = 0; i < cnt; i++)
    rh[i] |= lh[i];
}

void eval_bitint_bitxor(int32_t bits, void *lp, void *rp) {
  int32_t cnt = (bits + 63) / 64;
  uint64_t *lh = lp, *rh = rp;

  for (int32_t i = 0; i < cnt; i++)
    rh[i] ^= lh[i];
}

void eval_bitint_shl(int32_t bits, void *sp, void *dp, int32_t amount) {
  if (amount < 0 || amount >= bits)
    return;

  uint64_t *src = sp, *dst = dp;
  int32_t idx = (bits - 1) / 64;
  int32_t src_idx = idx - amount / 64;
  int32_t shft = amount % 64;

  uint64_t hi = src[src_idx--];
  for (; idx >= 0; idx--) {
    uint64_t lo = src_idx >= 0 ? src[src_idx--] : 0;
    dst[idx] = !shft ? hi : (hi << shft) | (lo >> (64 - shft));
    hi = lo;
  }
}

void eval_bitint_shr(int32_t bits, void *sp, void *dp, int32_t amount, bool is_unsigned) {
  if (amount < 0 || amount >= bits)
    return;

  uint64_t *src = sp, *dst = dp;
  int32_t src_idx = amount / 64;
  int32_t src_top = (bits - 1) / 64;
  int32_t top = (bits - 1 - amount) / 64;
  int32_t shft = amount % 64;

  uint64_t lo = src[src_idx++];
  for (int32_t idx = 0; idx <= top; idx++) {
    uint64_t hi = src_idx <= src_top ? src[src_idx++] : 0;
    dst[idx] = !shft ? lo : (hi << (64 - shft)) | (lo >> shft);
    lo = hi;
  }
  eval_bitint_sign_ext(bits - amount, dp, bits, is_unsigned);
}

void *eval_bitint_bitfield_load(int32_t bits, void *sp, void *dp, int32_t width, int32_t ofs, bool is_unsigned) {
  char *src = sp, *dst = dp;
  int32_t sz = (bits + 7) / 8;

  for (int32_t i = 0; i < sz; i++)
    dst[i] = src[i];

  eval_bitint_shl(bits, dp, dp, bits - width - ofs);
  eval_bitint_shr(bits, dp, dp, bits - width, is_unsigned);
  return sp;
}

void eval_bitint_bitfield_save(int32_t bits, void *sp, void *dp, int32_t width, int32_t ofs) {
  int32_t cnt = (bits + 63) / 64;
  int32_t full_bits = cnt * 64;

  uint64_t mb[cnt], sb[cnt];
  for (int32_t i = 0; i < cnt; i++)
    mb[i] = -1;

  eval_bitint_shl(full_bits, mb, mb, full_bits - width);
  eval_bitint_shl(full_bits, sp, sb, full_bits - width);

  eval_bitint_shr(full_bits, mb, mb, full_bits - width - ofs, true);
  eval_bitint_shr(full_bits, sb, sb, full_bits - width - ofs, true);

  eval_bitint_bitnot(full_bits, mb);

  char *dst = dp, *src = (char *)sb, *msk = (char *)mb;
  int32_t top = (width + ofs - 1) / 8;

  for (int32_t i = ofs / 8; i <= top; i++)
    dst[i] = (dst[i] & msk[i]) | src[i];
}

void eval_bitint_add(int32_t bits, void *lp, void *rp) {
  int32_t cnt = (bits + 63) / 64;
  uint64_t *lh = lp, *rh = rp;

  uint64_t carry = 0;
  for (int32_t i = 0; i < cnt; i++) {
    uint64_t tmp;
    carry = (tmp = rh[i] + carry) < carry;
    carry |= (rh[i] = lh[i] + tmp) < tmp;
  }
}

void eval_bitint_sub(int32_t bits, void *lp, void *rp) {
  int32_t cnt = (bits + 63) / 64;
  uint64_t *lh = lp, *rh = rp;

  uint64_t borrow = 0;
  for (int32_t i = 0; i < cnt; i++) {
    uint64_t tmp;
    borrow = (tmp = rh[i] + borrow) < borrow;
    borrow |= (rh[i] = lh[i] - tmp) > lh[i];
  }
}

void eval_bitint_mul(int32_t bits, void *lp, void *rp) {
  int32_t cnt = (bits + 31) / 32;
  uint32_t *lh = lp, *rh = rp;

  uint32_t buf[cnt];

  uint64_t accum = 0;
  for (int32_t i = 0; i < cnt; i++) {
    uint64_t ovf_cnt = 0;
    for (int32_t j = 0; j <= i; j++) {
      uint64_t prod = (uint64_t)lh[j] * rh[i - j];
      ovf_cnt += (accum += prod) < prod;
    }
    buf[i] = (uint32_t)accum;
    accum = ovf_cnt << 32 | accum >> 32;
  }
  for (int32_t i = 0; i < cnt; i++)
    rh[i] = buf[i];
}

void eval_bitint_div(int32_t bits, void *lp, void *rp, bool is_unsigned, bool is_div) {
  int32_t cnt = (bits + 63) / 64 * 2;
  uint32_t *lh = lp, *rh = rp;

  uint32_t r_buf[cnt + 2];
  uint32_t q_buf[cnt];

  for (int32_t i = 0; i < cnt; i++) {
    r_buf[i] = lh[i];
    q_buf[i] = 0;
  }
  r_buf[cnt] = r_buf[cnt + 1] = 0;

  int32_t msl = (bits - 1) / 32;
  int32_t sign_shft = (bits - 1) % 32;

  bool l_neg = is_unsigned ? 0 : (r_buf[msl] >> sign_shft) & 1;
  bool r_neg = is_unsigned ? 0 : (rh[msl] >> sign_shft) & 1;

  if (l_neg)
    eval_bitint_neg(bits, r_buf);
  if (r_neg)
    eval_bitint_neg(bits, rh);

  int32_t l_fsb = eval_bitint_first_set(bits, r_buf);
  int32_t r_fsb = eval_bitint_first_set(bits, rh);

  int32_t r_ofs = r_fsb % 32;
  int32_t r_shft = r_ofs ? 32 - r_ofs : 0;
  int32_t ln = (l_fsb + 31) / 32;
  int32_t rn = (r_fsb + 31) / 32;

  if (r_shft) {
    eval_bitint_shl(bits + 64, r_buf, r_buf, r_shft);
    eval_bitint_shl(bits, rh, rh, r_shft);
  }

  for (int32_t qi = ln - rn; qi >= 0; qi--) {
    uint32_t quo;
    if (r_buf[qi + rn] == rh[rn - 1]) {
      quo = -1;
    } else {
      uint64_t top = (uint64_t)r_buf[qi + rn] << 32 | r_buf[qi + rn - 1];
      uint64_t q = top / rh[rn - 1];
      uint64_t r = top % rh[rn - 1];

      if (rn > 1) {
        r = r << 32 | r_buf[qi + rn - 2];
        uint64_t r_inc = (uint64_t)rh[rn - 1] << 32;
        while (r < q * rh[rn - 2]) {
          q -= 1;
          if ((r += r_inc) < r_inc)
            break;
        }
      }
      quo = q;
    }

    uint32_t sub = 0;
    for (int32_t i = 0; i < rn; i++) {
      uint64_t prod = (uint64_t)quo * rh[i];

      uint32_t tmp = (uint32_t)prod + sub;
      sub = (tmp < sub) + (r_buf[qi + i] < tmp) + (prod >> 32);
      r_buf[qi + i] -= tmp;
    }

    uint32_t rem = r_buf[qi + rn] - sub;
    if (rem > r_buf[qi + rn]) {
      uint64_t carry = 0;
      for (int32_t i = 0; i < rn; i++) {
        uint64_t tmp;
        carry = (tmp = rh[i] + carry) < carry;
        carry |= (r_buf[qi + i] = r_buf[qi + i] + tmp) < tmp;
      }
      rem += carry;
      quo -= 1;
    }
    r_buf[qi + rn] = rem;
    q_buf[qi] = quo;
  }

  uint32_t *res;
  if (is_div) {
    if (l_neg + r_neg == 1)
      eval_bitint_neg(bits, q_buf);
    res = q_buf;
  } else {
    if (r_shft)
      eval_bitint_shr(bits + 64, r_buf, r_buf, r_shft, true);
    if (l_neg)
      eval_bitint_neg(bits, r_buf);
    res = r_buf;
  }
  for (int32_t i = 0; i < cnt; i++)
    rh[i] = res[i];
}

#include "slimcc.h"

int32_t eval_bitint_first_set(int32_t bits, BitBuf *op) {
  int32_t idx = (bits - 1) / 64;

  int clr_shft = 63 - (bits - 1) % 64;
  (&op->as64)[idx] = ((&op->as64)[idx] << clr_shft) >> clr_shft;

  for (; idx >= 0; idx--)
    if ((&op->as64)[idx])
      for (int shft = 0; shft < 64; shft++)
        if (0 > (int64_t)((&op->as64)[idx] << shft))
          return (64 - shft) + idx * 64;
  return 0;
}

bool eval_bitint_overflow(int32_t bits, BitBuf *src, int32_t chk_bits, bool is_unsigned) {
  int32_t idx = (chk_bits - 1) / 64;
  int shft = (chk_bits - 1) % 64;

  int64_t chk = (int64_t)(&src->as64)[idx++] >> shft;
  if (chk != 0)
    if (is_unsigned || chk != -1)
      return true;

  int32_t top = (bits - 1) / 64;
  while (idx <= top)
    if ((&src->as64)[idx++] != chk)
      return true;

  return false;
}

bool eval_bitint_to_bool(int32_t bits, BitBuf *op) {
  int32_t idx = (bits - 1) / 64;

  int clr_shft = 63 - (bits - 1) % 64;
  (&op->as64)[idx] = ((&op->as64)[idx] << clr_shft) >> clr_shft;

  while (idx >= 0)
    if ((&op->as64)[idx--])
      return true;
  return false;
}

int eval_bitint_cmp(int32_t bits, BitBuf *lh, BitBuf *rh, bool is_unsigned) {
  int32_t idx = (bits - 1) / 64;

  int clr_shft = 63 - (bits - 1) % 64;
  uint64_t signbit = is_unsigned ? 0 : 1ULL << 63;
  uint64_t lv = ((&lh->as64)[idx] << clr_shft) + signbit;
  uint64_t rv = ((&rh->as64)[idx] << clr_shft) + signbit;

  for (;;) {
    if (lv > rv)
      return 2;
    else if (lv < rv)
      return 1;

    if (--idx < 0)
      break;
    lv = (&lh->as64)[idx];
    rv = (&rh->as64)[idx];
  }
  return 0;
}

void eval_bitint_sign_ext(int32_t bits, BitBuf *op, int32_t bits2, bool is_unsigned) {
  int32_t idx = (bits - 1) / 64;

  uint64_t fill = (&op->as64)[idx];
  int shft = bits % 64;
  if (shft) {
    fill <<= 64 - shft;
    if (is_unsigned)
      (&op->as64)[idx] = (uint64_t)fill >> (64 - shft);
    else
      (&op->as64)[idx] = (int64_t)fill >> (64 - shft);
  }
  fill = is_unsigned ? 0 : (int64_t)fill >> 63;

  int32_t top = (bits2 - 1) / 64;
  while (++idx <= top)
    (&op->as64)[idx] = fill;
}

void eval_bitint_neg(int32_t bits, BitBuf *op) {
  int32_t cnt = (bits + 63) / 64;

  uint64_t borrow = 0;
  for (int32_t i = 0; i < cnt; i++) {
    uint64_t tmp;
    borrow = (tmp = (&op->as64)[i] + borrow) < borrow;
    borrow |= ((&op->as64)[i] = 0 - tmp) > 0;
  }
}

void eval_bitint_bitnot(int32_t bits, BitBuf *op) {
  int32_t cnt = (bits + 63) / 64;

  for (int32_t i = 0; i < cnt; i++)
    (&op->as64)[i] = ~(&op->as64)[i];
}

void eval_bitint_bitand(int32_t bits, BitBuf *lh, BitBuf *rh) {
  int32_t cnt = (bits + 63) / 64;

  for (int32_t i = 0; i < cnt; i++)
    (&rh->as64)[i] &= (&lh->as64)[i];
}

void eval_bitint_bitor(int32_t bits, BitBuf *lh, BitBuf *rh) {
  int32_t cnt = (bits + 63) / 64;

  for (int32_t i = 0; i < cnt; i++)
    (&rh->as64)[i] |= (&lh->as64)[i];
}

void eval_bitint_bitxor(int32_t bits, BitBuf *lh, BitBuf *rh) {
  int32_t cnt = (bits + 63) / 64;

  for (int32_t i = 0; i < cnt; i++)
    (&rh->as64)[i] ^= (&lh->as64)[i];
}

void eval_bitint_shl(int32_t bits, BitBuf *src, BitBuf *dst, int32_t amount) {
  if (amount < 0 || amount >= bits)
    return;

  int32_t idx = (bits - 1) / 64;
  int32_t src_idx = idx - amount / 64;
  int32_t shft = amount % 64;

  uint64_t hi = (&src->as64)[src_idx--];
  for (; idx >= 0; idx--) {
    uint64_t lo = src_idx >= 0 ? (&src->as64)[src_idx--] : 0;
    (&dst->as64)[idx] = !shft ? hi : (hi << shft) | (lo >> (64 - shft));
    hi = lo;
  }
}

void eval_bitint_shr(int32_t bits, BitBuf *src, BitBuf *dst, int32_t amount,
                     bool is_unsigned) {
  if (amount < 0 || amount >= bits)
    return;

  int32_t src_idx = amount / 64;
  int32_t src_top = (bits - 1) / 64;
  int32_t top = (bits - 1 - amount) / 64;
  int32_t shft = amount % 64;

  uint64_t lo = (&src->as64)[src_idx++];
  for (int32_t idx = 0; idx <= top; idx++) {
    uint64_t hi = src_idx <= src_top ? (&src->as64)[src_idx++] : 0;
    (&dst->as64)[idx] = !shft ? lo : (hi << (64 - shft)) | (lo >> shft);
    lo = hi;
  }
  eval_bitint_sign_ext(bits - amount, dst, bits, is_unsigned);
}

void *eval_bitint_bitfield_load(int32_t bits, BitBuf *src, BitBuf *dst, int32_t width,
                                int32_t ofs, bool is_unsigned) {
  int32_t sz = (bits + 7) / 8;

  for (int32_t i = 0; i < sz; i++)
    (&dst->as8)[i] = (&src->as8)[i];

  eval_bitint_shl(bits, dst, dst, bits - width - ofs);
  eval_bitint_shr(bits, dst, dst, bits - width, is_unsigned);
  return src;
}

void eval_bitint_bitfield_save(int32_t bits, BitBuf *src, BitBuf *dst, int32_t width,
                               int32_t ofs) {
#ifndef BOOTSTRAP_NO_VLA
  int32_t cnt = (bits + 63) / 64;
  int32_t full_bits = cnt * 64;

  BitBuf mb[cnt], sb[cnt];
  for (int32_t i = 0; i < cnt; i++)
    (&mb->as64)[i] = -1;

  eval_bitint_shl(full_bits, mb, mb, full_bits - width);
  eval_bitint_shl(full_bits, src, sb, full_bits - width);

  eval_bitint_shr(full_bits, mb, mb, full_bits - width - ofs, true);
  eval_bitint_shr(full_bits, sb, sb, full_bits - width - ofs, true);

  eval_bitint_bitnot(full_bits, mb);

  int32_t top = (width + ofs - 1) / 8;

  for (int32_t j = ofs / 8; j <= top; j++)
    (&dst->as8)[j] = ((&dst->as8)[j] & (&mb->as8)[j]) | (&sb->as8)[j];
#endif
}

void eval_bitint_add(int32_t bits, BitBuf *lh, BitBuf *rh) {
  int32_t cnt = (bits + 63) / 64;

  uint64_t carry = 0;
  for (int32_t i = 0; i < cnt; i++) {
    uint64_t tmp;
    carry = (tmp = (&rh->as64)[i] + carry) < carry;
    carry |= ((&rh->as64)[i] = (&lh->as64)[i] + tmp) < tmp;
  }
}

void eval_bitint_sub(int32_t bits, BitBuf *lh, BitBuf *rh) {
  int32_t cnt = (bits + 63) / 64;

  uint64_t borrow = 0;
  for (int32_t i = 0; i < cnt; i++) {
    uint64_t tmp;
    borrow = (tmp = (&rh->as64)[i] + borrow) < borrow;
    borrow |= ((&rh->as64)[i] = (&lh->as64)[i] - tmp) > (&lh->as64)[i];
  }
}

void eval_bitint_mul(int32_t bits, BitBuf *lh, BitBuf *rh) {
#ifndef BOOTSTRAP_NO_VLA
  int32_t cnt = (bits + 31) / 32;

  uint32_t buf[cnt];

  uint64_t accum = 0;
  for (int32_t i = 0; i < cnt; i++) {
    uint64_t ovf_cnt = 0;
    for (int32_t j = 0; j <= i; j++) {
      uint64_t prod = (uint64_t)(&lh->as32)[j] * (&rh->as32)[i - j];
      ovf_cnt += (accum += prod) < prod;
    }
    buf[i] = (uint32_t)accum;
    accum = ovf_cnt << 32 | accum >> 32;
  }
  for (int32_t j = 0; j < cnt; j++)
    (&rh->as32)[j] = buf[j];
#endif
}

void eval_bitint_div(int32_t bits, BitBuf *lh, BitBuf *rh, bool is_unsigned, bool is_div) {
#ifndef BOOTSTRAP_NO_VLA
  int32_t cnt64 = (bits + 63) / 64;

  BitBuf lb[cnt64 + 1];

  for (int32_t i = 0; i < cnt64; i++) {
    (&lb->as64)[i] = (&lh->as64)[i];
    (&lh->as64)[i] = 0;
  }
  (&lb->as64)[cnt64] = 0;

  int32_t msl = (bits - 1) / 32;
  int32_t sign_shft = (bits - 1) % 32;

  bool l_neg = is_unsigned ? 0 : ((&lb->as32)[msl] >> sign_shft) & 1;
  bool r_neg = is_unsigned ? 0 : ((&rh->as32)[msl] >> sign_shft) & 1;

  if (l_neg)
    eval_bitint_neg(bits, lb);
  if (r_neg)
    eval_bitint_neg(bits, rh);

  int32_t l_fsb = eval_bitint_first_set(bits, lb);
  int32_t r_fsb = eval_bitint_first_set(bits, rh);

  int32_t r_ofs = r_fsb % 32;
  int32_t r_shft = r_ofs ? 32 - r_ofs : 0;
  int32_t ln = (l_fsb + 31) / 32;
  int32_t rn = (r_fsb + 31) / 32;

  if (r_shft) {
    eval_bitint_shl(bits + 64, lb, lb, r_shft);
    eval_bitint_shl(rn * 32, rh, rh, r_shft);
  }

  for (int32_t qi = ln - rn; qi >= 0; qi--) {
    uint32_t quo;
    if ((&lb->as32)[qi + rn] == (&rh->as32)[rn - 1]) {
      quo = -1;
    } else {
      uint64_t top = (uint64_t)(&lb->as32)[qi + rn] << 32 | (&lb->as32)[qi + rn - 1];
      uint64_t q = top / (&rh->as32)[rn - 1];
      uint64_t r = top % (&rh->as32)[rn - 1];

      if (rn > 1) {
        r = r << 32 | (&lb->as32)[qi + rn - 2];
        uint64_t r_inc = (uint64_t)(&rh->as32)[rn - 1] << 32;
        while (r < q * (&rh->as32)[rn - 2]) {
          q -= 1;
          if ((r += r_inc) < r_inc)
            break;
        }
      }
      quo = q;
    }

    uint32_t sub = 0;
    for (int32_t i = 0; i < rn; i++) {
      uint64_t prod = (uint64_t)quo * (&rh->as32)[i];

      uint32_t tmp = (uint32_t)prod + sub;
      sub = (tmp < sub) + ((&lb->as32)[qi + i] < tmp) + (prod >> 32);
      (&lb->as32)[qi + i] -= tmp;
    }

    uint32_t rem = (&lb->as32)[qi + rn] - sub;
    if (rem > (&lb->as32)[qi + rn]) {
      uint64_t carry = 0;
      for (int32_t i = 0; i < rn; i++) {
        uint64_t tmp;
        carry = (tmp = (&rh->as32)[i] + carry) < carry;
        carry |= ((&lb->as32)[qi + i] = (&lb->as32)[qi + i] + tmp) < tmp;
      }
      rem += carry;
      quo -= 1;
    }
    (&lb->as32)[qi + rn] = rem;
    (&lh->as32)[qi] = quo;
  }

  BitBuf *res;
  if (is_div) {
    if (l_neg + r_neg == 1)
      eval_bitint_neg(bits, lh);
    res = lh;
  } else {
    if (r_shft)
      eval_bitint_shr(bits + 64, lb, lb, r_shft, true);
    if (l_neg)
      eval_bitint_neg(bits, lb);
    res = lb;
  }
  for (int32_t j = 0; j < cnt64; j++)
    (&rh->as64)[j] = (&res->as64)[j];
#endif
}
